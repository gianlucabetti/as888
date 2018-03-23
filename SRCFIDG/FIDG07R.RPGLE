000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200070115     H* FIDG07R *-----------------------------------------------------*
000300070112     H*            riceraca giri autotrasportatore
000400930225     H*---------------------------------------------------------------*
000500070112     FFidg07D   CF   E             WORKSTN
000600950525     F                                     SFILE(LRF0S03:NRR)
001600070928     Fazorg01L  IF   E           K DISK
001601070928     Ffiapd01L  IF   E           K DISK
001602061222     Ffiapd22L  IF   E           K DISK
001603070720     Ffiapd31L  IF   E           K DISK
001604070720     Ffiapd51L  IF   E           K DISK
002600030724     d dtaiso          s               d
002700030724     d dtaeur          s               d   datfmt(*eur)
004205070928     D og147         E DS
004206070928     D fidg07ds      E DS
004207070115     D fidg08ds      E DS
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
137510070720     c* verifico se ci sono giri con/ORM
137511070720     c     apd2nras      setll     fiapd31l                               44
137512070720     c     apd2nras      setll     fiapd51l                               45
137513070720     c                   select
137514070720     c                   when      *in44 and *in45
137515070830     c                   eval      vsctpd = 'C/R'
137516070720     c                   when      *in44 and not *in45
137517070720     c                   eval      vsctpd = 'CON'
137518070720     c                   when      not *in44 and *in45
137519070830     c                   eval      vsctpd = 'RIT'
137520070720     c                   other
137521070720     c                   clear                   vsctpd
137522070720     c                   endsl
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
146301070720     C     VSCSCE        IFEQ      'C'
146302070720     C     VSCSCE        orEQ      'R'
146303070112     C                   exsr      srdett
146304070112     C                   ENDIF
146305070115     c                   clear                   vscsce
146306070115     c                   update    LRF0S03
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
159908070720     c                   eval      d08tp= vscsce
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
159944070720     c                   select
159945070720     c                   when      vsctpd = 'CON'
159946070720     c                   eval      d07tpo = 'C'
159947070831     c                   when      vsctpd = 'RIT'
159948070720     c                   eval      d07tpo = 'R'
159949070720     c                   when      vsctpd = 'C/R'
159950070720     c                   eval      d07tpo = 'E'
159951070720     c                   other
159952070720     c                   clear                   d07tpo
159953070720     c                   endsl
159954070112     c                   movel     fidg07ds      kpjbu
159955070112     C                   SETON                                        LR
159956070112     c                   return
159957070112     C*
159958070112     C                   ENDSR
159959070112     C**********************************************************************
159960070112     C*--- INZSR INIZIALE --------------------------------------------*
159961070112     C**********************************************************************
159962070112     C     *INZSR        BEGSR
159963061002     C*
159964061002     C     *ENTRY        PLIST
159965061002     C                   PARM                    KPJBA
159966070112      * Reperisco dati job
159967070112     c                   exsr      DatiJob
159968070116     c*
159969070116     c                   eval      v1cpgm = 'FIDG07R'
159970070112     c*
159971070112     c                   if        kpjbu = ' '
159972070112     c                   clear                   fidg07ds
159974070928     c                   movel     dutpou        compdr
159975070112     c                   else
159976070112     C                   MOVEL     KPJBU         fidg07ds
159977070119     c                   clear                   d07pdro
159978070119     c                   clear                   d07ddeo
159979070119     c                   clear                   d07scado
159980070119     c                   clear                   d07fppo
159981070112     c                   end
159982070119     c                   if        d07tipa= ' '
159983070119     c                   eval      d07tipa= 'A'
159984070112     c                   end
159985070112     c                   if        d07pdr = 0 and d07fgs <> 0
159986070112     c                   movel     d07fgs        compdr
159987070112     c                   end
159988070112     c                   if        d07pdr <>0
159989070112     c                   movel     d07pdr        compdr
159990070112     c                   end
159991070928     c* verifico se abilitare ritiri
159993070928     c                   movel     compdr        orgfil
159996070928     c     orgfil        chain     azorg01l
159997070928     c                   if        %found(azorg01l)
159999070928     C                   MOVEL     ORGde7        og147
160000070928     c                   else
160001070928     c                   clear                   og147
160002070928     c                   end
160015070928     c     §OGCGIO       comp      ' '                                    51
160016061002
160017070112     c     Kapd          Klist
160018070119     c                   Kfld                    d07tipa
160019070112     c                   Kfld                    compdr            7 0
160020070112     c     Kapd2         Klist
160021070119     c                   Kfld                    apd2tipa
160022070112     c                   Kfld                    apd2pdr
160023061002     C                   ENDSR
160024061002      *---------------------------------------------------------------*
160025061002      * Reperimento Dati del job (Utente/Operativi)                   *
160026061002      *---------------------------------------------------------------*
160027061002     c     DatiJob       BEGSR
160028061002      *
160029061002     c     *dtaara       define    §azute        azuteds
160030061002     c     *dtaara       define    §datiute      ddatiute
160031061002      *
160032061002     c                   in(E)     *dtaara
160033061002     c                   IF        %ERROR or RSUT = *blanks
160034061002     c                   clear                   Tibs34Ds
160035061002     c                   call      'TIBS34R'
160036061002     c                   parm                    Tibs34Ds
160037061002     c                   in        *dtaara
160038061002     c                   ENDIF
160039061002      *
160040061002     c                   ENDSR
