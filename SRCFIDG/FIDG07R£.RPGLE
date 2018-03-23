000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200070115     H* FIDG07R *-----------------------------------------------------*
000300070112     H*            riceraca giri autotrasportatore
000400930225     H*---------------------------------------------------------------*
000500070112     FFidg07D   CF   E             WORKSTN
000600950525     F                                     SFILE(LRF0S03:NRR)
001600021203     Ffiapd01L  IF   E           K DISK
001601061222     Ffiapd22L  IF   E           K DISK
002600030724     d dtaiso          s               d
002700030724     d dtaeur          s               d   datfmt(*eur)
004205070112     D fidg07ds      E DS
004206070115     D fidg08ds      E DS
009100900514     D KPJBA         E DS
009101061002     d* Parametri x Controllo profilo utenti
009102061002     d TIBS34ds      e ds
009103061002     d AZUTEds       e ds                  extname(AZUTE00F)
009104061002     d* Ds per dati organigramma
009105061002     d DDatiUte      e ds
013919061002     C*
126500950525     C* PULIZIA SUBFILE
126600950525     C                   SETON                                        21
126700950525     C                   WRITE     LRF0C03
126800950525     C                   SETOFF                                       21
127000950525     C* CARICAMENTO SUBFILE
127100950525     C                   EXSR      RIES03
127300950525     C* SE NON E' STATO CARICATO NESSUN RECORD ESCO
127400950525     C     NRR           IFEQ      0
127401070119     c                   movel     fidg07ds      kpjbu
127402070112     c                   seton                                        lr
127500070112     C                   return
127600061002     C                   else
127900950525     C* EMISSIONE FORMATO
128000950525     C                   WRITE     LRF0Z04
128100061002     C                   do        *hival
128200950525     C                   EXFMT     LRF0C03
128400950525     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
128500950525     C                   CLEAR                   V2CMSG
128600950525     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
128700070112     C                   SETOFF                                       28
129100950607     C** F3 - FINE
129200070116    1C                   IF        *inkc or *inkl
129201070119     c                   movel     fidg07ds      kpjbu
129300070112     C                   seton                                        lr
129500070112     C                   return
129600950525    1C                   ENDIF
131200950525     C                   EXSR      CTRS03
131300061002     C   28              iter
132204061002     c                   enddo
132400950526     C* RITORNO ALLA VIDEATA INIZIALE (QUELLA DELLE SELEZIONI)
132501061002     c                   end
132502070112     C**********************************************************************
132503070112     C*--- CARICAMENTO SUBFILE ---------------------------------------*
132504070112     C**********************************************************************
132505070112     C     RIES03        BEGSR
133000950525     C                   Z-ADD     0             NRR               4 0
133001070112     c     kapd          setll     fiapd22l
133002070112     c                   do        *hival
133003070112     c                   if        d07pdr <> 0
133004070112     c     kapd          reade     fiapd22l
133005070112     c                   else
133006070119     c     d07tipa       reade     fiapd22l
133007070112     c                   end
133008070112     c                   if        %eof(fiapd22l)
133009070112     c                   leave
133010070112     c                   end
133011070112     C* annullati si/no
133012070112     c                   if        d07atb = 'N' and apd2atb <> ' '
133013070112     c                   iter
133014070112     c                   end
133015070112     c* controllo se aut di competenza p.o.
133016070112     c                   if        d07fgs <> 0 and d07pdr = 0
133017070112     c                   movel     apd2pdr       com03             3 0
133018070112     c                   if        com03 > d07fgs
133019070112     c                   leave
133020070112     c                   end
133021070112     c                   end
133022070112     c*
133023070112     C     kapd2         CHAIN     fiapd01l
133024070112     c                   if        not %found(fiapd01l) or
133025070112     c                             (%found(fiapd01l) and apdatb <> ' ')
133026070112     C                   iter
133027070112    1C                   ENDIF
136200950525     C                   CLEAR                   VSCSCE
136201070112     C                   MOVEL     apd2atb       VSCatb
136400070119     C                   MOVEL     APD2nras      VShnras
136401070115     C                   MOVEL     APDPDR        VSCPDR
136500950525     C                   MOVEL     APDRSC        VSDPDR
137505070112     C                   MOVEL     apd2fpp       VSCfpp
137506070112     C                   MOVE      apd2dde       dtaiso
137507070112     C                   MOVE      dtaiso        vscdde
137508070119     C                   MOVE      apd2dscad     dtaiso
137509070119     C                   MOVE      dtaiso        vscscad
139200950525     C*
139300950525     C                   ADD       1             NRR
139400950525     C                   WRITE     LRF0S03
140200950525    1C                   ENDDO
140300950525     C*
140400950525     C                   Z-ADD     1             REC
140500950525     C                   ENDSR
142601061002     C**********************************************************************
142700950525     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
142701061002     C**********************************************************************
142800950525     C     CTRS03        BEGSR
145400950526     C* LEGGO IL SFL PER SAPERE SE E' STATO SELEZIONATO ALMENO UN RECORD
145601070112     c                   do        *hival        nrr
145700070112     C     NRR           CHAIN     LRF0S03                            31
145800070112    1C                   if        *in31
145801070112     c                   leave
145802070112     c                   end
145900070112     C* SCE = "1" --> E' STATO SELEZIONATO UN RECORD DEL SFL
146000950526     C     VSCSCE        IFEQ      '1'
146100070112     C                   exsr      srfine
146300950526     C                   ENDIF
146301070112     C     VSCSCE        IFEQ      'D'
146302070112     C                   exsr      srdett
146303070112     C                   ENDIF
146304070115     c                   clear                   vscsce
146305070115     c                   update    LRF0S03
146700950526    1C                   ENDDO
150900950607     C*
151000070112     C                   ENDSR
159901061002     C**********************************************************************
159902070112     C*--- dettaglio giri --------------------------------------------*
159903061002     C**********************************************************************
159904070112     C     srdett        BEGSR
159905070112     C*
159906070115     c                   clear                   fidg08ds
159907070119     c                   eval      d08nras= vshnras
159916070115     c                   movel     fidg08ds      kpjbu
159917070115     c                   call      'FIDG08R'
159918070115     c                   parm                    kpjba
159919070115     c                   movel     kpjbu         fidg08ds
159931070112     C*
159932070112     C                   ENDSR
159933070112     C**********************************************************************
159934070112     C*--- fine ------------------------------------------------------*
159935070112     C**********************************************************************
159936070112     C     srfine        BEGSR
159937070112     C*
159938070119     c                   eval      d07pdro= vscpdr
159939070119     c                   eval      d07fppo= vscfpp
159940070119     c                   movel     vscdde        dtaiso
159941070119     c                   movel     dtaiso        d07ddeo
159942070119     c                   movel     vscscad       dtaiso
159943070119     c                   movel     dtaiso        d07scado
159944070112     c                   movel     fidg07ds      kpjbu
159945070112     C                   SETON                                        LR
159946070112     c                   return
159947070112     C*
159948070112     C                   ENDSR
159949070112     C**********************************************************************
159950070112     C*--- INZSR INIZIALE --------------------------------------------*
159951070112     C**********************************************************************
159952070112     C     *INZSR        BEGSR
159953061002     C*
159954061002     C     *ENTRY        PLIST
159955061002     C                   PARM                    KPJBA
159956070112      * Reperisco dati job
159957070112     c                   exsr      DatiJob
159958070116     c*
159959070116     c                   eval      v1cpgm = 'FIDG07R'
159960070112     c*
159961070112     c                   if        kpjbu = ' '
159962070112     c                   clear                   fidg07ds
159963070112     c                   else
159964070112     C                   MOVEL     KPJBU         fidg07ds
159965070119     c                   clear                   d07pdro
159966070119     c                   clear                   d07ddeo
159967070119     c                   clear                   d07scado
159968070119     c                   clear                   d07fppo
159969070112     c                   end
159970070119     c                   if        d07tipa= ' '
159971070119     c                   eval      d07tipa= 'A'
159972070112     c                   end
159973070112     c                   if        d07pdr = 0 and d07fgs <> 0
159974070112     c                   movel     d07fgs        compdr
159975070112     c                   end
159976070112     c                   if        d07pdr <>0
159977070112     c                   movel     d07pdr        compdr
159978070112     c                   end
159979061002
159980070112     c     Kapd          Klist
159981070119     c                   Kfld                    d07tipa
159982070112     c                   Kfld                    compdr            7 0
159983070112     c     Kapd2         Klist
159984070119     c                   Kfld                    apd2tipa
159985070112     c                   Kfld                    apd2pdr
160016061002     C                   ENDSR
160017061002      *---------------------------------------------------------------*
160018061002      * Reperimento Dati del job (Utente/Operativi)                   *
160019061002      *---------------------------------------------------------------*
160020061002     c     DatiJob       BEGSR
160021061002      *
160022061002     c     *dtaara       define    §azute        azuteds
160023061002     c     *dtaara       define    §datiute      ddatiute
160024061002      *
160025061002     c                   in(E)     *dtaara
160026061002     c                   IF        %ERROR or RSUT = *blanks
160027061002     c                   clear                   Tibs34Ds
160028061002     c                   call      'TIBS34R'
160029061002     c                   parm                    Tibs34Ds
160030061002     c                   in        *dtaara
160031061002     c                   ENDIF
160032061002      *
160033061002     c                   ENDSR
