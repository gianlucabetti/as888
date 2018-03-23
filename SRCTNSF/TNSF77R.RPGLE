000100050322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200170203     H*PARMS BNDDIR(PJXBND PJCBND YCO)
000300050322     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000301170203     H DFTACTGRP(*NO) ACTGRP('QILE')
000302170203     H BNDDIR('PJXBND':'PJCBND':'YCO')
000400170321     H/TITLE     -- RECUPERO FATTURAZIONE AUTOMATICA --                *
000500170321     H*----------------------------------------------------------------*
000501170622     H* Attaverso questo programma dovrei ripristinare la situazione   *
000502170321     H* della fatturazione a tutto come era prima del lancio a parte   *
000503170321     H* l'aggiornamneto in cnclp e ybacl della data prima e ultima     *
000504170321     H* spedizione fatturata.                                          *
000505170321     H*                                                                *
000506170321     H*----------------------------------------------------------------*
000800050322     H**
001800170321     Fndbhm00f  uf   e           k disk
001900170321     Fndbhi00f  uf   e           k disk
002000170321     Fndbha00f  uf   e           k disk
002200050429     Ftitas30c  uf   e           k disk    ignore(titas010)
002300050429     F                                     ignore(titasp00)
002400050429     F                                     rename(titas000:titas30)
002500170321     Ftita730c  uf   e           k disk    ignore(tita7p00)
002800170321     Fwfrtas1l  if   e           k disk    prefix(wf_)
003700170321     Ffiar531c  uf   e           k disk    Ignore(fiar5p00)
004000170321     Ftitai01l  uf   e           k disk
004100060227
012400991201
013200000000     D PARAM           DS
013300170321     D  NR_azione                     9  0
023000991223      **
023100000000     D KPJBA         E DS
100600170321      *
100601170321     d Dtasv         e ds
100602170321     d  sv                     1     20
100603170321     d                                     Dim(20)                              Sigle varie
100604170321     d  va                    21    140P 3
100605170321     d                                     Dim(20)                              Importi varie
100606170321      ** DS  VARIE del file WFTAS
100607170321     d Dtasvwf       e ds                  extname(dtasv) prefix(WF_)
100608170321     d  WF_sv                  1     20
100609170321     d                                     Dim(20)                              Sigle varie
100610170321     d  WF_va                 21    140P 3
100611170321     d                                     Dim(20)                              Importi varie
101600050404
101601170321      **********************************************************************************************
101602170321      *            C I C L O       L E T T U R A
101603170321      **********************************************************************************************
101604170321      *
101700050404     c     *loval        setll     wfrtas1l
101800050404
101900050411    1c                   do        *hival
102000050404     c                   read      wfrtas1l
102100050404
102200170322      * fine file
102300050404
102400050411    2c                   If        %eof(wfrtas1l)
102600050404     c                   leave
102700050411    2c                   endif
102800050404
114800050420      *
114900050411      * Aggancio TITAS
115000050429     c     Ktas30        chain     titas30c
115100050411      * se non lo trovo leggo il successivo (COMUNQUE E'''' IMPOSSIBBBBBBILE)
115101170322      *
115200050429    2c                   If        not %found(titas30c)
115300050411     c                   iter
115400050411    2c                   endif
115405170322      * deleto tutti i valori che sono stati memorizzati nel TITAI prima della tassazione
115406170322      *
115407170322     c     ktai          setll     titai01l
115408170609    2c                   do        *hival
115409170322     c     ktai          reade     titai01l
115410170609    3c                   if        %eof(titai01l)
115411170322     c                   leave
115412170609    3c                   endif
115413170322     c                   delete    titai00f
115414170609    2c                   enddo
115415170322      * ripristino in titas i valori che ho in WFRTAS
115416170322      *
115417170322     c                   eval      taspor= wf_taspor
115418170322     c                   eval      tasimv= wf_tasimv
115419170322     c                   eval      taspvl= wf_taspvl
115420170609     c                   eval      tasdiv= wf_tasdiv
115421170609      * ripristino flag peso usato in fatturazione
115422170609     c                   eval      tasfpf = W_TASfpf
115423170609      *            quantità da fatturare
115424170609     c                   eval      tasqft = W_TASqft
115425170609      *            flag esenzione IVA
115426170609     c                   eval      tasfei = W_TASfei
115427170609      * importo d'assicurare impostato in fatturazione
115428170609    2c                   if        tasfcm = 'F'
115429170609     c                   clear                   tasfcm
115430170609     c                   clear                   tasias
115431170609     c                   clear                   tasvas
115432170609    2c                   endif
115433170609      *
115434170427      * ripristino le varie
115435170427      *
115436170427     c                   eval      dtasv = dtasvwf
115437170427     c                   exsr      carta7
115438170322     c
115439170322      * se la divisa in wfrtas è uguale a blk vuol dire che prima la bolla non era
115440170322      * tassata in nessuna parte quindi deleto FIAR5 record "FAT"  altrimenti
115441170322      * non deleto FIAR5
115442170322      *
115443170609    2c                   if        wf_tasdiv = '   '
115444170322      *
115445170322     c                   eval      kAr5Trd  = 'FAT'
115446170322     c     kFiar5        Chain     Fiar531c
115447170609    3c                   If        %Found(Fiar531c)
115448170322     c                   delete    fiar5000
115449170609    3c                   EndIf
115451170609    2c                   Endif
115452170609      *
115453170609      * pulisco il flag di contabilizzazione, P.O. iva
115454170609      * e data numero fattura
115455170609     c                   clear                   tasfic
115456170609     c                   clear                   tasdft
115457170609     c                   clear                   tasnft
115458170609     c                   clear                   tasfiv
120800170609    1c                   enddo
120900050415
121000050415
121100000000     C     FINE          TAG
121200891025     C** AGGIORNAMENTO FILE N.FATTURA
121300970523     C     *INH5         IFEQ      *OFF
121400970523     C     *INH7         ANDEQ     *OFF
121500961029     C                   EXSR      AGGNUM
121600961104     C**
121700961104     C* STAMPO IL PRIMO E L'ULTIMO NUMERO FATTURA INIZIO MESE E FINE ME
121800961104     C     WINIZ         IFEQ      '1'
121900961104     C                   Z-ADD     NFTIM         WINULT            6 0
122000961104     C                   ENDIF
122100961104     C     WFINE         IFEQ      '1'
122200961104     C                   Z-ADD     NFTFM         WFIULT            6 0
122300961104     C                   ENDIF
122400961104     C                   EXCEPT    NUMFAT
122500970523     C                   ENDIF
122600000107     c**
122700000107     c* STAMPO I TOTALI GENERALI
122800000107     C                   IF        TOTGEN > 0
122900000124     c* se sono valorizzati anche le conversioni in mineta di conto le stampo
123000000124     c                   eval      *in40  = (totgencnt > 0)
123100050415      * imposto che la moneta di conto prevede i decimali
123200050415     C                   eval      *in42 = *on
123300000124     c*
123400000107     C                   EXCEPT    GEN
123500000107     C                   ENDIF
123600000107     C***
123700000107     C*
123800050415     C                   CLEAR                   dslv55
123900980105     C                   MOVEL     'C'           D55TLA
124000980105     C                   CALL      'FNLV55R'
124100050415     C                   PARM                    dslv55
124200990715     C*
124300990715     C                   CLEAR                   DSBS69
124400990715     C                   MOVEL     'C'           I69TLA
124500990715     C                   CALL      'TIBS69R'
124600990715     C                   PARM                    DSBS69
124700990715     C                   PARM                    DSACO
124800990715     C                   PARM                    DSIND
124900990715     C                   PARM                    DSCLP
125000990715     C                   PARM                    DSCLS
125100000103     C*
125200000103     C                   CLEAR                   DSBS02
125300000111     C                   MOVEL     'C'           T02TLA
125400000103     C                   CALL      'TIBS02R'
125500000103     C                   PARM                    KPJBA
125600000103     C                   PARM                    DSBS02
125700050413      *
125800050413      ** chiudo i files di stampa
125900050413     c                   If        %open(ftwcodfm)
126000050413     c                   close     ftwcodfm
126100050413     c                   endif
126200050413      *
126300050413     c                   If        %open(ftwcodim)
126400050413     c                   close     ftwcodim
126500050413     c                   endif
126600050413      *
126700050413     c                   If        %open(ftwcodds)
126800050413     c                   close     ftwcodds
126900050413     c                   endif
127000050413      *
127100050413     c                   If        %open(disspe)
127200050413     c                   close     disspe
127300050413     c                   endif
127400050421      *
127500050421     c                   If        %open(prtf198)
127600050421     c                   close     prtf198
127700050421     c                   endif
127800050421      *
127900050421     c                   If        %open(qsysprt)
128000050421     c                   close     qsysprt
128100050421     c                   endif
128200050421      *
128300050421     c                   If        %open(sysprt)
128400050421     c                   close     sysprt
128500050421     c                   endif
128600050413      *
128700120214      * Ora la prima nota viene lanciata fuori fatturazione
128800120214     c*****              exsr      primanota
128900060228      *
129000060228
129100120214      * Ora il manca tariffa viene lanciato fuori fatturazione
129200060228      * se fatturazione mensile stampo manca tariffa
129300120214     c***20              exsr      mancatar
129400050324      * fine fatturazione
129500050324     c     finefat       tag
129600000000     C                   SETON                                        LR
129700991223
129800050415      ****************************************************
129900050415      ** Fine elaborazione stampa fatture
130000050415      ****************************************************
130100050415     c     SR_finele     Begsr
130200050418      *
130300050418     c                   exsr      totcod
130400050415
130500050415      * totale distinte di spedizione da fare a fine stampa
130600050415     c                   if        tdsp > 0 or tdspd > 0
130700050415     c                   except    statds
130800050415     c                   endif
130900050415      *
131000050415     c                   endsr
131100050415
131200050415      ****************************************************
131300050415      ** Aggancio tabella FAT
131400050415      ****************************************************
131500050415     c     SR_tabfat     Begsr
131600050415
131700050415     c                   clear                   dsbs02
131800050415     c                   movel     'L'           T02tla
131900050415     c                   movel     'C'           T02mod
132000050415     c                   movel     knsif         t02sif
132100050415     c                   movel     'FAT'         t02cod
132200050415     c                   movel     'FAT'         t02ke1
132300050415     c                   call      'TIBS02R'
132400050415     c                   parm                    KPJBA
132500050415     c                   parm                    DSBS02
132600050415     c                   movel(P)  t02uni        DFAT
132700050415
132800050415     c                   Endsr
132900050325      ****************************************************
133000050325      ** Scrittura file di lavoro wfrtas0f
133100050325      ****************************************************
133200050325     c     Wrktas        Begsr
133300050325
133400050325     c                   clear                   wfrtas00
133500050325      * se c'è raggruppamento + codici di bollettazione in uno di fatturazione
133600050329      * imposto il codice raggruppamento con il codice di fatturazione
133700050427     c                   if        savragbol = 'S' and fatksc > 0
133800050329     c                   movel     fatksc        wf_tasrsc
133900050329     c                   else
134000050329     c                   movel     bolksc        wf_tasrsc
134100050329     c                   endif
134200050420      *
134300050420     c                   eval      wf_tasfrg = savragbol
134400050329      *
134500050329     c                   eval      wf_tasbsc = tasksc
134600050329     c                   eval      wf_tastbl = tastbl
134700050329     c                   eval      wf_tasaas = tasaas
134800050329     c                   eval      wf_tasmgs = tasmgs
134900050329     c                   eval      wf_taslnp = taslnp
135000050329     c                   eval      wf_tasnrs = tasnrs
135100050329     c                   eval      wf_tasnsp = tasnsp
135200050329     c                   eval      wf_taspvl = taspvl
135300050329      * tassazione
135400050329      * porto
135500050329     c                   eval      wf_tasdiv = tasdiv
135600050329     c                   eval      wf_taspor = taspor
135700050329      * varie
135800050329     c                   eval      Dtasvwf   = Dtasv
135900050329      * imponibile
136000170127     c                   eval      wf_tasimv = tasimv
136100120516      * verifico se presenti cig/cup  per i soli clienti che desiderano stampe separate
136200120516     c                   If        savfatsep <> ' '
136300120516      *
136400120516     c                   eval      §4trc = '4'
136500120516     c     Kta4          chain     tita430c
136600120516    1c                   If        %found(tita430c) and                         * codici Cup-CIG
136700120516     c                             ta4not <> *blanks
136800120516     c                   eval      wf_tasfsl = savfatsep
136900120516    1c                   endif
137000120516      *
137100120516    1c                   endif
137200120516      *
137300050329      * il record viene scritto solo se bolla valida per la fatturazione
137400050329     c                   endsr
137500050329      *************************************************
137600050329      **  Aggiorna archivio di lavoro WFRTAS
137700050329      *************************************************
137800050330     c     Aggwrf        Begsr
137900050330
138000050330      * valorizzo campi nel file di lavoro utili per la stampa
138100050330      * per aggiornare file assicurazione o per una eventuale ripartenza
138200050330
138300050330     c                   eval      wf_taspkcn = taspkcn
138400050330     c                   eval      wf_tasspc  = tasspc
138500050330     c                   eval      wf_tasncl  = stpncl
138501160315     c                   eval      wf_tasfie  = tasfie
138502160427     c                   eval      wf_tasvlfd = stavlf
138600170126      * valorizzo imponibile memorizzato su TITAS (o da tassazione o da titas se già valorizzato)
138601170126      * e esenzione che c'è in bolla
138602170126     c                   eval      wf_wtasimv = tasimv
138603170126      *
138604170126      * SE NON ESISTE NESSUNA ESENZIONE DELLA SPEDIZIONE
138605170126      * RECUPERO L'ESENZIONE IVA ASSOCIATA ALLA NAZIONE
138606170126     c                   IF        TASFEI = *BLANKS
138607170126     c                   Z-ADD     1             YY
138608170126     c                   SETOFF                                       43
138609170126     c     TASNZD        LOOKUP    NAZE(YY)                               43
138610170126     c   43              MOVEL     ESEN(YY)      TASFEI
138611170126     c                   ENDIF
138612170126     c                   eval      wf_wtasfei = tasfei
138700050330
138800050330     c                   z-add     1             a
138900050330     c     tastbl        lookup    tb(a)                                  05
139000050330     c   05a             occur     dstb
139100050330     c                   if        *in05 and §tbfas <> '0'
140900050330      ** Se l'imposto d'assicurare non c'era lo imposto e imposto anche il flag di messo in fatturaz
141000050330     c                   if        Tasias = 0
141100050330     c                   z-add     tasial        tasias
141200050330      *
141300050330     c                   if        tasial > 0
141400050330     c                   movel     'F'           Tasfcm
141500160315     c***!!***           eval      wf_tasfcm = 'F'
141600050330      * aggiunta valorizzazione della divisa dell'assicurazione con la stessa moneta del cliente
141700050330     c                   move      tasdiv        tasvas
141800050330     c                   endif
141900050330     c                   endif
142000050330      **
142100050330     c                   endif
142101160315      **
142200050330      * WFRTAS
142300050330     c                   write     wfrtas00
142400060428      *
142500060428      * verifico se la bolla era stata già tassata in tutto o in parte prima della
142600060428      * fatturazione
142700060502     c                   movea     WF_sv         comwf_sv
142800060502      * se sigle  delle varie diverso da blank o il porto maggiore di zero scrivo il
142900060502      * file in verticale per ogni importo o sigla varia valorizzato
143000060428      * la sigla del tipo importo per le varie è uguale alla sigla delle varie per il
143100060502      * porto è " ".
143200060428
143300060502     c                   If        comwf_sv <> *blanks or wf_taspor  > 0
143400060428      * imposto spedizione
143500060428     c                   clear                   titai000
143600060428     c                   eval      taiaas = wf_tasaas
143700060502     c                   eval      tailnp = wf_taslnp
143800060428     c                   eval      tainsp = wf_tasnsp
143900060428     c                   eval      tainrs = wf_tasnrs
144000060428     c                   eval      taitbl = wf_tastbl
144100060519      * Imponibile
144200060519     c                   If        wf_tasimv > 0
144300060519     c                   eval      taisvt = '£'
144400060519      * verifico se esiste il record
144500060519     c     ktai          setll     titai01l
144600060519     c                   If        not %equal(titai01l)
144700060519     c                   z-add     wf_tasimv     taivat
144800060519     c                   write     titai000
144900060519     c                   endif
145000060519     c                   endif
145100060428      * Porto
145200060428     c                   If        wf_taspor > 0
145300060502     c                   eval      taisvt = ' '
145400060502      * verifico se esiste il record
145500060502     c     ktai          setll     titai01l
145600060502     c                   If        not %equal(titai01l)
145700060428     c                   z-add     wf_taspor     taivat
145800060428     c                   write     titai000
145900060428     c                   endif
146000060502     c                   endif
146100060428      * Varie
146200060428     c                   Do        20            vv
146300060428     c                   If        wf_sv(vv)  <> ' '
146400060502     c                   eval      taisvt = wf_sv(vv)
146500060502      * verifico se esiste il record
146600060502     c     ktai          setll     titai01l
146700060502     c                   If        not %equal(titai01l)
146800060428     c                   z-add     wf_va(vv)     taivat
146900060428     c                   write     titai000
147000060502     c                   endif
147100060428     c                   endif
147200060428     c                   enddo
147300060428
147400060428     c                   endif
147500050330      *
147600050330     c                   endsr
147700050325      ****************************************************
147800170427      ** Carica importi varie su tita7000
147900050325      ****************************************************
148000050325     c     Carta7        Begsr
149200170427      * prima cancello tutti i record
149300050415     c     Kta7          setll     tita730c
149600050325      **
149700050325     c                   do        *hival
149800050415     c     Kta7          reade     tita730c
150000050415     c                   if        %eof(tita730c)
150100050325     c                   leave
150101170427     c                   else
150102170427     c                   delete    tita7000
150200050325     c                   endif
150800050325     c                   enddo
150900050325
151000050325     c                   endif
151001170427
151002170427      * carico le varie su tita7 presenti prima della tassazione
151003170427
151004170427     c                   If        sv(4) <> *blanks
151005170427     c                   do        20            k
151006170427     c                   If        k > 3  and sv(k) <> *blanks
151007170427     c                   z-add     tasaas        ta7aas
151008170427     c                   z-add     taslnp        ta7lnp
151009170427     c                   movel     tasnrs        ta7nrs
151010170427     c                   z-add     tasnsp        ta7nsp
151011170427     c                   movel     tastbl        ta7tbl
151012170427     c                   movel     sv(k)         ta7svn
151013170427     c                   z-add     va(k)         ta7van
151014170427     c                   write     Tita7000
151015170427     c                   endif
151016170427     c                   enddo
151017170427     c                   Endif
151300050325
151400050325     c                   Endsr
151500050331      ****************************************************
151600050331      ** Riverca Contrassegno
151700050331      ****************************************************
151800050331     c     Carcsb        Begsr
151900050331
152000050415     c     Kcsb          chain     Tncsb03l
152100050415     c                   If        %found(tncsb03l) and
152200050331      * solo per stesso tipo bolla
152300050419     c                             tastbl = csbtbl
152400050331     c                   movel     csbsta        Tassta
152500050331     c                   z-add     csbcas        Tascas
152600061016     c******             movel     csbtpa        Tastic
152700050331     c                   if        csbfus <> *blanks
152800050331     c                   move      csbfus        tastic
152900050331     c                   else
153000050331     c                   move      csbtpi        tastic
153100050331     c                   end
153200050331      * mittente
153300050331     c                   movel     csbvca        tasvca
153400050331     c                   z-add     csbcmb        tascmb
153500050331
153600050331     c                   endif
153700050331
153800050331     c                   endsr
153900050331      ***********************************************
154000050331      ** Aggiornamento bolle non contabilizzabili
154100050331      ***********************************************
154200050331     c     Conbol        Begsr
154300050331
154400050331      * aggiorno flag di tassazione / contabilizzazione / data fattura (inizio mese per
154500050331      * la mensile e data fattura per la fattura settimanale)
154600050331     c                   movel     '1'           tasfts
154700050331     c                   movel     '1'           tasfic
154800050331     c   20              z-add     dtdfi8        tasdft
154900050331     c  N20              z-add     dtdft8        tasdft
155000050331     c                   update    titas000
155100050331
155200050331     c                   endsr
155300050331      ****************************************************
155400050331      ** Tassazione Bolla
155500050331      ****************************************************
155600050331     c     Tassa         Begsr
155700050331
155800050331      * Clearizzo campi "VARIABILI" di DTAS
155900050331
156000050331     c                   movel     *Blanks       tastla
156100050331     c                   movel     *Blanks       taserr
156200050331     c                   movel     *Blanks       tasmsg
156201160427     c                   movel     *Blanks       tasfvfd
156300050331     c                   movel     Tasftc        tastc1
156400050331     c                   clear                   tasial
156500050331     c                   clear                   task88
156600050331     c                   clear                   tasfcaa
156700050331     c                   clear                   taspkcn
156800050929     c                   clear                   tasric
156900050331     c                   clear                   tasspc
157000050331     c                   clear                   tasBan
157001151124     c                   clear                   tasspl
157002151124     c                   clear                   tasprt
157003151124     c                   clear                   tasprg
157004151202     c                   clear                   tastap
157005151202     c                   clear                   tasepd
157006160610     c                   clear                   tasfvt
157007160610     c                   clear                   tasvlt
157008160531     c                   clear                   TasNclb
157100050331     c                   clear                   SavNcl
157200050331     c                   clear                   StpNcl
157201160427     c                   clear                   Stapkf
157202160427     c                   clear                   Stavlf
157300050331     c                   clear                   dAr5Ban
157400050331     c                   Clear                   dAr5Bnb
157500150909      * Clearizzo campi "VARIABILI" di DTAS
157600150909
157700150909     c                   Clear                   dtaspes
157701160601      * valorizzo il numro colli bollettati se ci dovessero essere il numero dei colli originali
157702160601      * il campo tasncl conterrà quest'ultimo valore
157703160531     c                   z-add     tasncl        tasnclb
157800050331      *
157900050331      * Per la fatturazione     MENSILE uso la data fatturazione FINE MESE
158000050331      *
158100050331      * Per la fatturazione NON MENSILE uso la data fattura
158200050331      *
158300050331      *     Per cariacre la tariffa di cartello decorrente
158400050331      *
158500050331     c   20              Z-add     dtdff8        tasdct
158600050331     c  N20              Z-add     dtdft8        tasdct
158700050331
158800050331      * Flag SI NO DDT
158900050331    1C                   If        tasll1 = 'C' or tasll1 = 'S'
159000050331     C                   movel     'S'           Tasddt
159100050331   x1C                   Else
159200050331     C                   Clear                   Tasddt
159300050331    1C                   Endif
159400060922
159500060922      * valorizzo flag bolla di rientro e la consegna anomala da passare al pgm di tassazione
159600060922
159700060922     c******             movel     tasfbr        dstasflo
159800060922     c                   clear                   dtas01
159900060922     c                   movel     tasfbr        §asfbr
160000060922     c                   movel     tascca        §ascca
160100110629      * valorizzo flag email al destinatario
160200110629     c                   movel     tasflo        dtasflo
160300110629     c                   move      §floemd       §asemd
160314151124      **
160400150112      /free
160500150112       //?Imposto se part.consegna con PinCode
160600150112         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
160700150112           §ASpin = 'S';
160800150112         ENDIF;
160900150112      /end-free
161000110629
161100060922     c                   eval      dstasflo = dtas01
161101151124
161102160107      * se non sto tassando una seconda bolla memorizzo i flag di diritto di chiamata e
161103160107      * packing list
161104160107     c                   if        secondabolla = ' '
161105151124      * valorizzo il campo per l'addebito diritto di chiamata prenotazione ritiro
161106151124     c                   move      §floado       tasprt
161107151124      * valorizzo il campo per l'addebito stampa PACKING LIST
161108151124     c                   eval      kAr5Trd  = 'GEN'
161109151124     c                   clear                   DAr5gen
161110151124     c     kFiar5        Chain(n)  Fiar531c
161111160427     c                   If        %Found(Fiar531c)
161112151124     c                   Movel     Ar5Uni        dAr5Gen
161113160427     c                   EndIf
161114151124      *
161115151124     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
161116151124     c                   Eval      TasSpl = 'S'
161117151124     c                   endif
161118160107
161119160107     c                   endif
161200050331
161300050331      * Bancali
161400160427     c                   If        %Subst(TasGva:1:1) = 'B'
161500050331
161600050331     c                   eval      kAr5Trd  = 'BAN'
161700150910     c     kFiar5        Chain(n)  Fiar531c
161800160427     c                   If        %Found(Fiar531c)
161900050331     c                   Movel     Ar5Uni        dAr5Ban
162000160427     c                   EndIf
162100050331      * Bancali
162200050331     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
162300160427     c                   EndIf
162400050331      **
162500050331      * Colli Originali
162600160427     c                   If        %Subst(TasGva:1:1) = 'O'
162700050331
162800050331     c                   eval      SavNcl = TasNcl
162900050331     c                   eval      kAr5Trd  = 'BNB'
163000150910     c     kFiar5        Chain(n)  Fiar531c
163100160427     c                   If        %Found(Fiar531c)
163200050331     c                   Movel     Ar5Uni        dAr5Bnb
163300160427     c                   EndIf
163400050331      * Colli
163500050331     c                   Eval      TasNcl = §Ar5bcor
163600050331     c                   Eval      StpNcl = §Ar5bcor
163700160427     c                   EndIf
163800050331      **
163900050331     c                   Movel     Paramt        Kpjbu
164000050331      *
164100050331      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
164200050331      * con VARIA 'N' valorizzata con 888888 chiamo il TNSF20R per il calcolo dell'anteporto
164300050331      *
164400160427     c                   if        tasimv = 0 and tasfbr = 'S'
164500050331     c                   z-add     1             xx
164600050331     c     'N'           lookup    sv(xx)                                 30
164700160427     c                   if        *in30 and va(xx) =  88888888
164800050331      * tasso l'anteporto
164900050331     c                   exsr      Varian
165000160427     c                   endif
165100050331      *
165200160427     c                   endif
165300050331      *
165400050331      * se ritorno dal calcolo della varia N e c'è errore non proseguo
165500050331      *
165600050331     c                   Setoff                                       05
165700050331    1c                   if        taserr = ' '
165800050506      * Se devo tassare per una varia e c'e' gia', non richiamo la tassazione  se importo
165900050506      * della varia uguale  a zero
166000050331    2c                   if        tassva<>*blanks and tasimv=0
166100050506     c                   z-add     1             kk
166200050506     c     tassva        lookup    sv(kk)                                 05
166300050506     c                   if        *in05 and va(kk) > 0
166400050506     c                   setoff                                       05
166500050506     c                   endif
166600050331    2c                   endif
166700050331      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
166800050331      * TNSF20R il flag di calcolo varia &
166900050331      *
167000050331     c                   setoff                                       51
167100050331     c                   z-add     1             kk
167200050331     c     '&'           lookup    sv(kk)                                 51
167300050331    2c                   if        *in51 and va(kk) = 88888888
167400050331     c                   eval      tasfcaa = 'S'
167500050331     c                   clear                   sv(kk)
167600050331     c                   clear                   va(kk)
167700050331    2c                   endif
167800050929      * verifico se devo calcolare l'addebito di lasciato avviso
167900050929      * verifico se c'è evento
168000080618     c     Kevb4         chain     Fnevb04l
168100050929
168200080618     c                   If        %found(Fnevb04l)
168300080618     c                   eval      tasric = 'S'
168400080618     c                   endif
168500050929
168600050929
168700050331      **
168800050331      ** DATA FATT X CARIC.DATI TARIFFA CARTELLO 1° VOLTA
168900050331    2c                   if        tassva=*blanks or not *in05
168901151106
168902160427    3c                   if        tasimv = 0
169000160427      * Passo peso bollettato,data tassazione, flag volume da fatturare alla ds DTASPES
169100150909     c                   eval      tasppkb = taspkb
169200150917     c                   eval      taspdtt = dateu
169201160426     c                   eval      taspfvf = tasfvf
169202160426     c                   eval      taspncr = tasncr
169300150909
169400150909     C                   call      'TNSF22R'
169500050331     C                   parm                    Kpjba
169600050331     C                   parm                    Dtas
169700050331     C                   parm                    Dtasv
169800150909     C                   parm                    Dtaspes
169801160427
169802160531      * se al ritorno dalla tassazione nella ds DTAS se è valorizzato il flag e il volume
169803160531      * usato in tassazione , uso quello in stampa altrimenti lo determino
169804160531     c                   if        tasfvt <> ' '
169805160531     c                   eval      stavlf = tasvlt
169806160531     c                   else
169807160427      * valorizzo i flag e il volume nel caso in cui sia desunto da DTASPES
169808160429     c                   if        tasvdes = 'S'
169809160427     c                   eval      stavlf=tasvvld
169810160427     c                   else
169811160427     c                   eval      stavlf = tasvlf
169812160427     c                   endif
169813160531
169814160531     c                   endif
170701151106
170702151106      * imponibile maggiore di zero (bolla pretassata)
170703160427  x 3c                   else
170704151106      * devo verficare se ha il FIAR5 record FAT .... peso e volumi usati in tassazione
170705151106     c                   eval      kAr5Trd  = 'FAT'
170706151106     c                   clear                   dAr5Fat
170707151106     c     kFiar5        Chain(n)  Fiar531c
170708160427     c                   If        %Found(Fiar531c)
170709151106     c                   Movel     Ar5Uni        dAr5Fat
170710160427     c                   EndIf
170711151106
170712160427      * se presente record FAT sostitisco i pesi/volumi usati in tassazione a quelli presenti
170713160427      * su TITAS solo momentaneamente
170714160427    4c                   if        §AR5PKTAS > 0
170715151106     c                   eval      s_dtas = dtas
170716151106     c                   eval      s_dtasv = dtasv
170718151106     c                   eval      taspkf = §AR5PKTAS
170719151106     c                   eval      tasvlf=§AR5VLTAS
170720160427     c                   eval      tasfvfD =§AR5fvTAS
170721160610     c                   eval      tasvlt=§AR5VLTAS
170722160610     c                   eval      tasfvt  =§AR5fvTAS
170723151106     c                   clear                   taspkc
170724151106     c                   clear                   tasncp
170725160531     c                   clear                   tasncr
170726160531     c                   clear                   tasvlc
170727160427    4c                   endif
170728151106
170729151106     c                   call      'TNSF20R'
170730151106     c                   PARM                    KPJBA
170731151106     c                   PARM                    DTAS
170732151106     c                   PARM                    Dtasv
170734151106     c
170735151106      * ripristino i valori modificati   e valorizzo quelli che mi interessano
170736151106      * importo d'assicurare e la sua divisa solo se presente record FAT
170737151106
170738160427    4c                   if        §AR5PKTAS > 0
170739151106      * valori assicurazione
170740151106     c                   eval      sv_tasial = tasial
170741151106     c                   eval      sv_tasdiv = tasdiv
170742160426      * valori volume desunto li recupero dal record FAT
170743160531      * ag 31/05/16 stampo sempre il volume usato in tassazione
170744160531     c***                if        tasfvfd = 'D'
170745160427     c                   eval      stavlf = §ar5vltas
170746160531     c***                else
170747160531     c***                eval      stavlf = tasvlf
170748160531     c**                 endif
170749151106      * valori ds dtas e dtasv
170750151106     c                   eval      dtas = s_dtas
170751151106     c                   eval      dtasv = s_dtasv
170752151106     c                   eval      tasdiv = sv_tasdiv
170753151106     c                   eval      tasial = sv_tasial
170754151106      *
170755151106     c                   if        tasfpf = 'D'
170756151106     c                   eval      tasspc = 'D'
170757151106     c                   eval      taspkcn = §AR5PKTAS
170758151106     c                   endif
170759151106      *
170760151106     c                   if        tasfpf = 'V'
170761151106     c                   eval      tasspc = 'S'
170762151106     c                   eval      taspkcn = §AR5PKTAS
170763151106     c                   endif
170764151106      *
170765160427    4c                   endif
170766151106
170767160427    3c                   endif
170768151204
170769151204      * se non ho tassato con il peso vld e il peso desunto imposto il flag tasfpf = 'R'
170770151204    3c                   if        Tasspc=' '
170771151204     c                   eval      tasfpf='R'
170772151204    3c                   endif
170773151106      * se ho tassato con il peso vld imposto il flag tasfpf
170774151106    3c                   if        Tasspc='S'
170775151106     c                   eval      tasfpf='V'
170776151106    3c                   endif
170777151106      * se ho tassato con il peso desunto imposto il flag tasfpf
170778151106    3c                   if        Tasspc='D'
170779151106     c                   eval      tasfpf='D'
170780151106    3c                   endif
170800050331      *
170900050331     c* se ho utilizzato i colli originali per tassare ripristino quelli del file titas
171000050331      * Colli Originali
171100050331    1c                   If        %Subst(TasGva:1:1) = 'O' and savncl > 0
171200050331     c                   eval      TasNcl = SavNcl
171300050331     c                   endif
171400050419
171500150911     c                   If        tasspc= 'S'  or tasspc = 'D'
171600050419      * mi salvo il peso da stampare
171700050419     c                   eval      stapkf=TasPkcN
171800050419     c                   else
171900050419     c                   eval      stapkf=taspkf
172000050419     c                   endif
172100050331      *
172200050331    2c                   endif
172300050331    1c                   endif
172400050331      **
172500050331     c                   Endsr
172600050331      ***************************************************
172700050331      ** Calcolo anteporto VARIA 'N'
172800050331      ***************************************************
172900050331     c     Varian        Begsr
173000050331
173100050331      * richiamo il programma di tassazione
173200050331      * senza passare le varie e il porto
173300050331      *
173400050331      * mi salvo le ds di tassazione
173500050331     c                   eval      w_dtas = dtas
173600050331     c                   eval      w_dtasv = dtasv
173700050331      * mi salvo la divisa
173800050331     c                   eval      w_tasdiv = tasdiv
173900050331
174000110701      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
174100110701     c                   eval      dtas01 = dstasflo
174200110701     c                   clear                   §asemd
174300150112      * tolgo il flag della consegna pin code  nell'eventualità ci fosse
174400150112     c                   clear                   §aspin
174500150112
174600110701     c                   eval      dstasflo = dtas01
174601160113      * tolgo flag per calcolo  Diritto di chiamata prenotazione  ritiro e packing list
174602160113      *
174603160113     c                   clear                   TASPRT
174604160113     c                   clear                   TASSPL
174700110701
174800050331     c                   clear                   dtasv
174900050331     c                   clear                   tasdiv
175000050331     c                   clear                   taspor
175100050331
175200050331     c                   clear                   antepo
175300050331
175400050331     c                   call      'TNSF20R'
175500050331     c                   parm                    kpjba
175600050331     c                   parm                    dtas
175700050331     c                   parm                    dtasv
175800050331      * se non ci sono errori valorizzo la varia 'N'
175900050331     c                   if        taserr = ' '
176000050331     c                   z-add     tasimv        antepo
176100050331     c                   endif
176200050331      * ripristino le ds salvate prima
176300050331     c                   eval      dtas = w_dtas
176400050331     c                   eval      dtasv = w_dtasv
176500050331      * valorizzo l'anteporto nella varia N al posto di 88888888  se maggiore di zero
176600050331     c                   if        antepo > 0
176700050331     c                   z-add     antepo        va(xx)
176800050331     c                   endif
176900050331
177000050331     c                   endsr
177100050331      *************************************************
177200050331      **  Aggiorna spedizione Titas Tita7
177300050331      *************************************************
177400050331     c     Aggbol        Begsr
177500050331
177600050331      ** Se era già tassata e l'imponibile è uguale al precedente non
177700050331      ** aggiorno Tita7
177800050331     c                   if        preimv <> tasimv
177900050331
178000050331      ** Cancellazione di tutti i record esistenti prima della scrittura
178100050331
178200050331     c                   If        Flgta7 <> 0
178300050415     c     Kta7          setll     Tita7000
178400050331     c                   do        *hival
178500050415     c     Kta7          Reade     Tita730c
178600050415     c                   if        %eof(tita730c)
178700050331     c                   leave
178800050331     c                   endif
178900050415     c                   delete    Tita730c
179000050331     c                   enddo
179100050331     c                   endif
179200050331
179300050331      * carico le varie su tita7
179400050331
179500050331     c                   If        sv(4) <> *blanks
179600050331     c                   do        20            k
179700050331     c                   If        k > 3  and sv(k) <> *blanks
179800050331     c                   z-add     tasaas        ta7aas
179900050331     c                   z-add     taslnp        ta7lnp
180000050331     c                   movel     tasnrs        ta7nrs
180100050331     c                   z-add     tasnsp        ta7nsp
180200050331     c                   movel     tastbl        ta7tbl
180300050331     c                   movel     sv(k)         ta7svn
180400050331     c                   z-add     va(k)         ta7van
180500050415     c                   write     Tita7000
180600050331     c                   endif
180700050331     c                   enddo
180800050331     c                   Endif
180900050331
181000050331     c                   endif
181100050331
181200050331     c                   Endsr
181300050331      **************************************************
181400050331      ** Stampa Errori Manca tariffa
181500050331      *************************************************
181600050331     c     Errore        Begsr
181700050419
181800150910     c                   If        tasspc= 'S' or tasspc = 'D'
181900060228      * mi salvo il peso da stampare
182000060228     c                   eval      stapkf=TasPkcN
182100060228     c                   else
182200060228     c                   eval      stapkf=taspkf
182300060228     c                   endif
182301160603      * se flag volume usato in tassazione è diverso da blank lo stampo
182302160603     c                   if        tasfvt <> ' '
182303160603     c                   eval      stavlf = tasvlt
182304160603     c                   else
182305160427      * se il flag di volume desunto non è valorizzato stampo il volume di TITAS
182306160427     c                   If        tasfvfd= ' '
182307160427     c                   eval      stavlf=tasvlf
182310160427     c                   endif
182400060228      *
182401160603     c                   endif
182402160603      *
182500060228     c                   clear                   Wfmtc000
182600060228
182700060228     c                   eval      Mtcdel = dateu
182800060228     c                   eval      Mtcksc = tasksc
182900060228     c                   eval      Mtcpoc = savfil
183000060228     c                   eval      Mtccar = Savcar
183100060228      * se cliente codificato
183200060228     c                   if        not *in10
183300060228     c                   eval      Mtcrag = taarsc
183400060228     c                   else
183500060228     c                   eval      Mtcrag = Bolra1
183600060228     c                   endif
183700060228
183800060228     c                   eval      Mtcage = Savage
183900060228     c                   eval      Mtcaas = Tasaas
184000060228     c                   eval      Mtclnp = Taslnp
184100060228     c                   eval      Mtcnrs = Tasnrs
184200060228     c                   eval      Mtcnsp = Tasnsp
184300060228     c                   eval      Mtctbl = Tastbl
184400060228     c                   movel     tasaas        Mtcdsp
184500060228     c                   move      tasmgs        Mtcdsp
184600090327     c                   if        Stpncl > 0
184700090327     c                   eval      Mtcncl = Stpncl
184800090327     c                   eval      %subst(Mtcesc:1:1) = 'o'
184900090327     c                   else
185000060228     c                   eval      Mtcncl = Tasncl
185100090327     c                   endif
185200060228     c                   eval      Mtcpkf = Stapkf
185300160427     c                   eval      Mtcvlf = Stavlf
185400060228     c                   eval      Mtcfap = Tasfap
185500060228     c                   eval      Mtclna = Taslna
185600060228     c                   eval      Mtccts = Tascts
185700060228     c                   eval      Mtcfin = Tasfin
185800060301      * recupero la provincia
185900060301     c                   z-add     1             a
186000060301     c                   movel     *blanks       wctprv
186100060301     c     tascts        lookup    cts(a)                                 05
186200060301     c   05a             occur     dsct
186300060301      ** recupero provincia codice tassazione
186400060301     c   05              movel     §ctprv        wctprv
186500060228     c                   eval      Mtcpro = wctprv
186600060228     c                   eval      Mtctsp = Tastsp
186700060228     c                   eval      Mtcctr = Tasctr
186800060228     c                   eval      Mtcdiv = Tasdiv
186900060228     c                   eval      Mtcpor = Taspor
187000060228     c                   eval      Mtcimv = Tasimv
187100060228
187200060228     c                   if        not *in95 and not *in99
187300060228     c                   eval      Mtccas = Tascas
187400060228     c                   eval      Mtcvca = Tasvca
187500060228     c                   eval      Mtcbap = Tasbap
187600060228     c                   endif
187700060228
187800050331      ** valorizzo il messaggio di errore in base all'errore da stampare
187900050331
188000060301      * se manca tariffa
188100060301     c                   If        *in94
188200060301     c                   If        tasmsg <> *blanks
188300060301     c                   movel     tasmsg        mtcerr
188400060301     c                   else
188500050331      * se manca tariffa e non c'è un msg risultante ne impostiamo uno fisso
188600060228     c                   movel     e(6)          mtcerr
188700050331     c                   endif
188800060301     c                   endif
188900060228
189000060228     C   99              movel(p)  e(1)          mtcerr
189100060228     C   95              movel(p)  e(3)          mtcerr
189200050331      *
189300050331      * imponibile maggiore del massimo fatturabile
189400050331      *
189500050331     c                   If        *in04
189600050331     c                   eval      w017a = %editc(wimv:'2')
189700050331      * preparo il campo del messaggio
189800060228     c                   clear                   mtcerr
189900060228     c                   eval      mtcerr = 'IMPONIBILE ' + tasdiv + ' ' +
190000050331     c                             %trim(w017a) + ' MAGGIORE DEL LIMITE ' +
190100050331     c                             'FATTURABILE'
190200050331     c                   endif
190300120910      *
190400120910      * imponibile minore o uguale a zero
190500120910      *
190600120910     c                   If        *in03
190700120910     c                   clear                   mtcerr
190800120910     c                   eval      mtcerr = 'IMPONIBILE A ZERO'
190900120910     c                   endif
191000120910      *
191100050331      * se è  un 8888 sempre manca tariffa
191200050331     c                   if        Ksccod = 8888
191300060228     c                   movel(p)  e(8)          mtcerr
191400050331     c                   endif
191500060228
191600060228     c                   write     Wfmtc000
191700060228
191800050331     c                   endsr
191900150909      **************************************************
192000150909      ** Registrazione veriazione peso Desunto
192100150909      *************************************************
192200150909     c     Desunto       Begsr
192300150909      *
192400150909     c                   clear                   Wfvpd000
192500150909
192600150909     c                   eval      Vpdksc = tasksc
192700150909     c                   eval      Vpdaas = Tasaas
192800150909     c                   eval      Vpdlnp = Taslnp
192900150909     c                   eval      Vpdnrs = Tasnrs
193000150909     c                   eval      Vpdnsp = Tasnsp
193100150909     c                   eval      Vpdtbl = Tastbl
193200150909     c                   movel     tasaas        Vpddsp
193300150909     c                   move      tasmgs        Vpddsp
193400150909     c                   eval      Vpdpkf = Taspkf
193500150909     c                   eval      Vpdpkb = Taspkb
193600150909     c                   eval      VpdPdes    = TASPDES
193700150909     c                   eval      VpdPPKD    = TASPPKD
193800150909     c                   eval      VpdPANNO   = TASPANNO
193900150909     c                   eval      VpdPMESE   = TASPMESE
194000150909     c                   eval      VpdPCOLLI  = TASPCOLLI
194100150909     c                   eval      VpdPSCOST  = TASPSCOST
194200150909     c                   eval      VpdPNSP    = TASPNSP
194300150909     c                   eval      VpdPIMV    = TASPIMV
194301160426     c                   eval      Vpdvncr = Taspncr
194302160426     c                   eval      Vpdvfvf = Tasfvf
194303160426     c                   eval      Vpdvvlf = Tasvlf
194305160426     c                   eval      VpdVdes    = TASVDES
194306160426     c                   eval      VpdVVLM    = TASVVLD
194307160426     c                   eval      VpdVANNO   = TASVANNO
194308160426     c                   eval      VpdVMESE   = TASVMESE
194309160426     c                   eval      VpdVCOLLI  = TASVCOLLI
194310160426     c                   eval      VpdVSCOST  = TASVSCOST
194311160426     c                   eval      VpdVNSP    = TASVNSP
194312160427     c                   eval      VpdVIMVD   = TASPIMVD
194400150909     c                   eval      VpdPERR    = TASPERR
194500150909     c                   eval      VpdPMSG    = TASPMSG
194501160608      * riporto quanto impostato nella ds DTAS sul volume usato in tassazione
194502160603     c                   eval      Vpdfvt     = tasfvt
194503160603     c                   eval      Vpdvlt     = tasvlt
194504160608      * imposto i valori di ritorno da TNSF22R nella DTASPES
194505160608     c                   eval      vpdvlmvdl = tasvlmvdl
194506160608     c                   eval      vpdvimvvr = taspimvvr
194600150909
194700150909     c                   write     Wfvpd000
194800150909
194900150909     c                   endsr
195000151008      **************************************************
195100151008      ** Registrazione record FAT del file AR5
195200151008      *************************************************
195300151008     c     AggAr5Fat     Begsr
195400151008      *
195500151008      * Memorizzo il peso che ho usato in tassazione nel fiar5 tipo record FAT
195600151008      * SE TASimv > 0 e non si tratta di seconda bolla
195700151008     c                   If        tasimv > 0  and tasimv <> preimv
195800151008
195900151008     c                   eval      kAr5Trd  = 'FAT'
196000151008     c     kFiar5        Chain     Fiar531c
196100151008If  1c                   If        %Found(Fiar531c)
196200151008     c                   Movel     Ar5Uni        dAr5Fat
196300151008     c                   else
196400151008     c                   clear                   dAr5Fat
196500151008     c                   clear                   FIAR5000
196600151008    1c                   EndIf
196700151008     c* Volume
196701160427      * se ho usato il desunto lo imposto  da dtaspes
196704160613     c*****              if        tasvdes = 'S'
196705160613     c*****              z-add     tasvvld       §ar5vltas
196706160613     c*****              movel     'D'           §ar5fvtas
196707160613     c*****              else
196708160613      * altrimenti lo recupero dalla DTAS (dovrebbe essere uguale)
196709160613     c                   z-add     tasvlt        §ar5vltas
196710160613     c                   movel     tasfvt        §ar5fvtas
196711160613     c*****              endif
197000151008     c* Peso se non utilizzato ne il desunto e neppure vdl metto R reale
197100151008     c                   if        tasspc = ' '
197200151008     c                   movel     'R'           §ar5fptas
197300151008     c                   z-add     taspkf        §ar5pktas
197400151008     c                   endif
197500151008     c* Peso se utilizzato il desunto metto D desunto
197600151008     c                   if        tasspc = 'D'
197700151008     c                   movel     'D'           §ar5fptas
197800151008     c                   z-add     taspkcn       §ar5pktas
197900151008     c                   endif
198000151008     c
198100151008     c* Peso se utilizzato quello VDL
198200151008     c                   if        tasspc = 'S'
198300151008     c                   z-add     taspkcn       §ar5pktas
198400151008    3c                   if        tasncp=tasncl
198500151008     c                   movel     'T'           §ar5fptas
198600151008     c                   else
198700151008     c                   movel     'Z'           §ar5fptas
198800151008    3c                   endif
198900151008     c
199000151008    2c                   endif
199100151008      * data e ora tassazione pc e utente
199200151008     c                   eval      §ar5HMtas = %dec(%time())
199300151008     c                   eval      §ar5DAtas = Dateu
199400151008     c                   eval      §ar5PRtas = Knmus
199500151008     c                   eval      §ar5NJtas = Knmtd
199600151008      * se peso desunto imposto anno e mese file statistica pesi SIPES
199601160427     c                   If        tasspc = 'D'
199800151008     c                   eval      §ar5STAAA  = TASPANNO
199900151008     c                   eval      §ar5STAMM  = TASPMESE
200000151008     c                   Else
200100151008     c                   clear                   §ar5STAAA
200200151008     c                   clear                   §ar5STAMM
200300151008     c                   Endif
200301160427      * se volume desunto imposto anno e mese file statistica pesi SIVLM
200302160427     c                   If        tasvdes = 'S'
200303160615     c                   eval      §ar5SVAAA  = TASVANNO
200304160615     c                   eval      §ar5SVAMM  = TASVMESE
200305160427     c                   eval      §ar5FVF = tasfvf
200306160427     c                   eval      §ar5VLF = tasvlf
200308160427     c                   Else
200309160615     c                   clear                   §ar5SVAAA
200310160615     c                   clear                   §ar5SVAMM
200311160427     c                   clear                   §ar5FVF
200312160427     c                   clear                   §ar5VLF
200313160427     c                   Endif
200314151124      * valorizzo il progressivo
200315151124     c                   movel     tasprg        §ar5PrgTa
200400151013
200500151008     c                   Movel     dAr5Fat       Ar5uni
200600151013      * imposto i campi di trasmissione
200700151013     c                   eval      ar5ft1= 'T'
200800151013     c                   eval      ar5dt1 = *all'9'
200900151013     c                   eval      ar5ft2= 'T'
201000151013     c                   eval      ar5dt2 = *all'9'
201100151013     c                   eval      ar5ft3= 'T'
201200151013     c                   eval      ar5dt3 = *all'9'
201300151013
201400151008If  1c                   If        %Found(Fiar531c)
201500151008     c                   update    fiar5000
201600151008     c                   else
201700151008     C                   eval      ar5aas=tasaas
201800151008     C                   eval      ar5lnp=taslnp
201900151008     C                   eval      ar5nrs=tasnrs
202000151008     C                   eval      ar5nsp=tasnsp
202100151008     c*
202200151008     C                   eval      ar5trd='FAT'
202300151008     c                   eval      ar5dac=DATEU
202400151008     c                   time                    ar5orc
202500151008     c                   write     fiar5000
202600151008    1c                   EndIf
202700151008
202800151008     c                   Endif
202900151008
203000151008     c                   Endsr
203100151008      *
203200050413      **************************************************
203300050413      ** Stampa Errori Contabili
203400050413      *************************************************
203500050413     c     Errcon        Begsr
203600050413      *
203700050413      **
203800050413      ** Testata del manca Tariffa
203900050413     c                   If        prima = ' '
204000050413     c                   Except    testcon
204100050413     c                   z-add     10            crcon             3 0
204200050413     c                   movel     'N'           prima             1
204300050413     c                   endif
204400050413      ** Nuova filiale cambio pagina
204500050413      **
204600050413     c                   If        crcon > 62
204700050413     c                   Except    Testcon
204800050413     c                   z-add     10            cr
204900050413     c                   endif
205000050413      **
205100050413     c                   If        *in34
205200050413     c                   except    detfat
205300050413     c                   else
205400050413     c                   except    detcon
205500050413     c                   endif
205600050413      **
205700050413     c                   add       2             cr
205800050413      **
205900050413     c                   endsr
206000050408      ********************************************************
206100050408      ** Memorizzazione Anagrafica cliente intestazione fattura
206200050408      ********************************************************
206300050408     c     Memana        begsr
206400050408
206500050411     c                   clear                   wlbeti
206600050411     c                   move      clnKCC        fatkcc
206700050411     c                   move      clnKSC        fatksc
206800050411     c                   movel     fatksc        fatfil
206900060707      * cerco terminal di partenza
207000060707      * chiamo Fnlv55 con tipo terminal "O" = Ottico
207100050415     c                   clear                   dslv55
207200060707     c                   movel     'O'           d55tpt
207300060707     c****               movel     'P'           d55tpt
207400050411     c                   moveL     fatfil        d55lin
207500050411     c                   movel     dateu         d55drf
207600050411     C                   call      'FNLV55R'
207700050415     c                   parm                    dslv55
207800050411     c                   movel     d55tfp        asfat
207900050411
208000050411     c                   movel     sogdes        fatra1                         RAG.SOCIALE
208100050504     c                   movel     sogdes        fatra2                         PER 2 INTEST
208200050411     c                   movel     aclTFT        fattft                         TIPO FATTURA
208300050411
208400050411     c                   movel     acltdf        fatifm                         INIZIO/FINE MESE
208500050411     c                   move      clncondpag    fatcdp                         CODPAGAMENTO
208600050411     c                   z-add     clnscopag1    fatsc1               16        SCONTO CASSA
208700050411     c                   z-add     clngginpa1    fatip1                         INIZIO PAG
208800050411     c                   z-add     clngginpa2    fatip2                          ==     ==
208900050411     c                   movel     indindriz     fatvia                         VIA
209000050411     c                   movel     indlocalit    fatcit                         CITTA
209100050411     c                   movel     indcap        fatcap                         CAP
209200050411     c                   movel     indstato      fatsta                         NAZIONE
209300050411
209400050411      * Se nazione estera stampo il cap a nove
209500050411     c                   IF        fatsta <> *blanks and fatsta <> 'I' and
209600050411     c                             fatsta <> 'IT'
209700050411     c                   movel     indcap        fatcae
209800050411     c                   seton                                        01
209900050411     c                   else
210000050411     c                   setoff                                       01
210100050411     c                   endif
210200050411
210300050411     c                   movel     sogpartiva    fativa                         P.IVA
210400061108      * codice fiscle
210500061108     c                   movel     sogcdfisc     fatcdf
210600061108      * se codice fiscale a blank lo imposto uguale alla partita iva
210700061108     c                   if        fatcdf = *blanks
210800061108     c                   eval      fatcdf = fativa
210900061108     c                   endif
211000061108
211100050411     c                   movel     indprov       fatprv                         PROVINCIA
211200050411     c     kcba          chain     ancba01l
211300050411     c                   if        %found(ancba01l)
211400050411     c                   movel     *blank        fatbna
211500050411     c                   clear                   X59Abids
211600050411     c                   move      cbasocieta    x59soc
211700050411     c                   move      cbaabi        x59ABI
211800050411     c                   move      Cbacab        x59CAB
211900050411     c                   CALL      'X59ABI'
212000050411     c                   Parm                    x59abids
212100050411     c                   if        x59err<>'1'
212200050411     c                   movel(p)  x59desist     fatbna                         BANCA APP.
212300050411     c                   movel(p)  x59desbrag    wwwage                         BANCA APP.
212400050411     c                   move      wwwage        fatbna                         BANCA APP.
212500981021     c                   endif
212600981021     c                   endif
212700050411     c                   clear                   fatscit
212800050411     c                   clear                   fatsci
212900050411     c                   clear                   fatali
213000050411     c                   if        clncdiva <> *blank
213100050411     c                   move      clncdiva      fatsci
213200050411     c                   move      clncdiva      fatscit
213300050411     c                   else
213400050411     c                   move      §qtali        fatali
213500050411     c                   endif
213600050411     c                   exsr      protocollo
213700050411     c                   clear                   staese
213800050411
213900050411     c                   movel     clnspeboll    fatsbl                         SPESE BOLLO
214000050411      ** condizioni di pagamento
214100050411     c     keycdp        chain     ancdp01l
214200050411     c                   if        %found(ancdp01l)
214300050411     c                   movel     cdpdesbrev    wfades
214400050411     c                   else
214500050411     c                   move      *blank        wfades
214600050411     c                   endif
214700050411     c                   if        cdpmdpag <> '1'
214800050411     c                   movel     *blank        fatbna
214900050411     c                   endif
215000050411
215100011129      *
215200050411     c                   ENDSR
215300050408      ********************************************************************
215400050408      ** Memorizzazione diritto fatturazione dal cliente di raggruppamento
215500050408      ********************************************************************
215600050408     c     memdirfat     Begsr
215700030804      * salvo il diritto di fatturazione
215800050408     c                   move      aclSIN        DIRfatt                        diritto di fatturazi
215900030922      * se diritto di fatturazione è uguale a zero imposto flag di calcolo uguale "N"
216000030922      * e il flag di stampa uguale a "N"
216100030922     c                   If        DIRfatt = 0
216200030922     c                   eval      Cal_vard = 'N'
216300030922     c                   eval      Stp_vard = 'N'
216400030922     c                   endif
216500050408      *
216600050408     c                   endsr
216700050408      ********************************************************************
216800050408      ** Memorizzazione Anagrafica Clienti Raggruppamento / bollettazione
216900050408      ********************************************************************
217000050404     c     Membol        Begsr
217100030617      *
217200050404     c                   movel     sogdes        bolra1                         RAG.SOCIALE
217300050404     c                   movel     indindriz     bolvia                         VIA
217400050404     c                   movel     indlocalit    bolcit                         CITTA
217500050404     c                   movel     indcap        bolcap                         CAP
217600050404     c                   movel     indprov       bolprv                         PROVINCIA
217700050404     c                   movel     indstato      bolsta                         NAZIONE
217800050404      * Se nazione estera stampo il cap a nove
217900050404     c                   If        bolsta <> *blanks and bolsta <> 'I'  and
218000050404     c                             bolsta <> 'IT'
218100050404     c                   movel     indcap        bolcae
218200000110     c                   seton                                        15
218300050404     c                   endif
218400060214      * Frequenza fattura
218500060214     c                   eval      fatfft = aclfft
218600060228      * codice agente
218700060228     c                   eval      savage = aclage
218800050404      *
218900050404     c                   Endsr
219000050420      ********************************************************************
219100050420      ** Memorizzazione Anagrafica Clienti bollettazione
219200050420      ********************************************************************
219300050420     c     Membsc        Begsr
219400050421      *
219500050421     c                   setoff                                       16
219600050421     c                   move      *zeros        ktasbsc
219700050421     c                   move      wf_tasbsc     ktasbsc
219800050420      *
219900050421     c     Keybol        chain     ancln01l
220000050421     c     Clnsogg       chain     ansog01l
220100050421     c     Keyind        chain     anind01l
220200050420
220300050420     c                   movel     sogdes        bscra1                         RAG.SOCIALE
220400050420     c                   movel     indindriz     bscvia                         VIA
220500050420     c                   movel     indlocalit    bsccit                         CITTA
220600050421     c                   movel     indcap        bsccap                         CAP
220700050420     c                   movel     indprov       bscprv                         PROVINCIA
220800050420     c                   movel     indstato      bscsta                         NAZIONE
220900050421      * Se nazione estera stampo il cap a nove
221000050421     c                   If        bscsta <> *blanks and bscsta <> 'I'  and
221100050421     c                             bscsta <> 'IT'
221200050421     c                   movel     indcap        bsccae
221300050421     c                   seton                                        16
221400050421     c                   endif
221500050421      * velorizzo il fatto ceh lo devo stampare
221600050421     c                   eval      wstbsc = 'Y'
221700050421      * mi preparo la ds con  i dati del codice
221800050421     c                   clear                   dssktot
221900050421     c                   eval      tksc = wf_tasbsc
222000050421     c                   eval      trsc = bscra1
222100050421      *
222200050420      * reimposto i dati  di prima
222300050420     c     Keycli        chain     ancln01l                           99
222400050420     c     Clnsogg       chain     ansog01l                           99
222500050420     c     Keyind        chain     anind01l                           99
222600050420
222700050420     c                   endsr
222800050405      ***************************************************
222900050405      ** Calcolo bollo se esente IVA
223000050405      ***************************************************
223100050405     c     Calbol        Begsr
223200050405
223300050405      * recupero i valori per il calcolo del bollo
223400050405
223500050405     c                   If        impbold >= §geimb
223600050405     c                   z-add     §geibl        totbold
223700050405     c                   endif
223800050405
223900050405     c                   Endsr
224000050421      *****************************************************
224100050421      ** Stampa totale per codice di bollettazione
224200050421      *****************************************************
224300050421     c     SR_Totbol     begsr
224400050421      *
224500050421      * diritto di fatturazione
224600050421      * se deve essere ancora stampato lo faccio ora
224601161024      * ASTERISSCO TUTTO IN QUANTO VIENE STAMPATO SOTTO LA BOLLA SULLA QUALE SI
224602161024      * E' CALCOLATO
224700161024     c****               if        stp_vard = ' '
224800050421      ** salto pagina
224900161024     c****               exsr      salpag
225000050421      *
225100161024     c****               select
225200050421      ** autofattura
225300161024     c**** fattft        wheneq    1
225400161024     c****               except    ddvdd2
225500050421      ** Fattura inizio mese
225600161024     c**** fatfiv        wheneq    §qcfiv
225700161024     c****               except    ddvdi2
225800050421      ** Fattura fine mese
225900161024     c****               other
226000161024     c****               except    ddvdf2
226100161024     c****               endsl
226200050421      *
226300161024     c****               eval      stp_vard = 'N'
226400161024     c****               endif
226500050421
226600050421      **  STAMPO I TOTALI
226700050422      ** 1 riga vuota
226800050421      ** salto pagina
226900050421     c                   exsr      salpag
227000050620
227100050620      * mi salvo i valori in schiera
227200050620     c                   add       1             itot
227300050620     c                   z-add     page          tpgf
227400050620     c                   eval      sktot(itot) = dssktot
227500050620      * preparo campi di stampa
227600050620     c                   z-add     tass          tasss
227700050620     c                   z-add     tfra          tfras
227800050620     c                   z-add     timp          timps
227900050421      *
228000050421     c                   select
228100050421      ** autofattura
228200050421     c     fattft        wheneq    1
228300050422     c                   except    tbodd0
228400050421      ** Fattura inizio mese
228500050421     c     fatfiv        wheneq    §qcfiv
228600050422     c                   except    tbodi0
228700050421      ** Fattura fine mese
228800050421     c                   other
228900050422     c                   except    tbodf0
229000050421     c                   endsl
229100050422      ** salto pagina
229200050422     c                   exsr      salpag
229300050422      *
229400050422     c                   select
229500050422      ** autofattura
229600050422     c     fattft        wheneq    1
229700050422     c                   except    tbodd1
229800050422      ** Fattura inizio mese
229900050422     c     fatfiv        wheneq    §qcfiv
230000050422     c                   except    tbodi1
230100050422      ** Fattura fine mese
230200050422     c                   other
230300050422     c                   except    tbodf1
230400050422     c                   endsl
230500050422      ** 1 riga vuota
230600050422      ** salto pagina
230700050422     c                   exsr      salpag
230800050422      *
230900050422     c                   select
231000050422      ** autofattura
231100050422     c     fattft        wheneq    1
231200050422     c                   except    tbodd0
231300050422      ** Fattura inizio mese
231400050422     c     fatfiv        wheneq    §qcfiv
231500050422     c                   except    tbodi0
231600050422      ** Fattura fine mese
231700050422     c                   other
231800050422     c                   except    tbodf0
231900050422     c                   endsl
232000050422      ** 1 riga vuota
232100050422      ** salto pagina
232200050422     c                   exsr      salpag
232300050422      *
232400050422     c                   select
232500050422      ** autofattura
232600050422     c     fattft        wheneq    1
232700050422     c                   except    tbodd0
232800050422      ** Fattura inizio mese
232900050422     c     fatfiv        wheneq    §qcfiv
233000050422     c                   except    tbodi0
233100050422      ** Fattura fine mese
233200050422     c                   other
233300050422     c                   except    tbodf0
233400050422     c                   endsl
233500050421
233600050421     c                   clear                   dssktot
233700050509
233800050509      * Aggancio YBACL con codiec di bollettazione
233900050509
234000050509     c                   move      tasbscpre     clnksc
234100050509
234200050509     c     Keybac        chain     ybacl01l
234300050509
234400050509     c                   exsr      aggbcl
234500050421      *
234600050421     c                   ENDSR
234700050421      *****************************************************
234800050421      ** Stampa riepilogo totale per codice di bollettazione
234900050421      *****************************************************
235000050421     c     SR_Riepil     begsr
235100050421
235200050421      **  STAMPO I TOTALI
235300050421
235400050421      ** salto pagina
235500050421     c                   exsr      salpag
235600050421      *
235700050421     c                   select
235800050421      ** autofattura
235900050421     c     fattft        wheneq    1
236000050421     c                   except    tbodd2
236100050421      ** Fattura inizio mese
236200050421     c     fatfiv        wheneq    §qcfiv
236300050421     c                   except    tbodi2
236400050421      ** Fattura fine mese
236500050421     c                   other
236600050421     c                   except    tbodf2
236700050421     c                   endsl
236800050421
236900050421      * stampo i valori in schiera
237000050421      * elaboro ogni elemenmto della schiera
237100050421
237200050421     c                   z-add     1             itotw
237300050421     c                   clear                   tasstw
237400050421     c                   clear                   tfratw
237500050421     c                   clear                   timptw
237600050421     c                   Dow       itotw<= itot
237700050421     c                   eval      dssktot = sktot(itotw)
237800050422      * preparo campi di stampa
237900050422     c                   z-add     tass          tasss
238000050422     c                   z-add     tfra          tfras
238100050422     c                   z-add     timp          timps
238200050422
238300050421
238400050421      **  STAMPO I TOTALI
238500050421
238600050421      ** salto pagina
238700050421     c                   exsr      salpag
238800050421      *
238900050421     c                   select
239000050421      ** autofattura
239100050421     c     fattft        wheneq    1
239200050421     c                   except    tbodd1
239300050421      ** Fattura inizio mese
239400050421     c     fatfiv        wheneq    §qcfiv
239500050421     c                   except    tbodi1
239600050421      ** Fattura fine mese
239700050421     c                   other
239800050421     c                   except    tbodf1
239900050421     c                   endsl
240000050620      ** salto pagina
240100050620     c                   exsr      salpag
240200050620      *
240300050620     c                   select
240400050620      ** autofattura
240500050620     c     fattft        wheneq    1
240600050620     c                   except    tbodd1P
240700050620      ** Fattura inizio mese
240800050620     c     fatfiv        wheneq    §qcfiv
240900050620     c                   except    tbodi1P
241000050620      ** Fattura fine mese
241100050620     c                   other
241200050620     c                   except    tbodf1P
241300050620     c                   endsl
241400050421
241500050421     c                   add       tass          tasstw
241600050421     c                   add       tfra          tfratw
241700050421     c                   add       timp          timptw
241800050421
241900050421     c                   add       1             itotw
242000050421
242100050421     c                   enddo
242200050421      *
242300050421      * stampo la riga dei totali riepilogo
242400050421      *
242500050421     c                   z-add     tasstw        tassst
242600050421     c                   z-add     tfratw        tfrast
242700050421     c                   z-add     timptw        timpst
242800050421
242900050421      ** salto pagina
243000050421     c                   exsr      salpag
243100050421      *
243200050421     c                   select
243300050421      ** autofattura
243400050421     c     fattft        wheneq    1
243500050421     c                   except    tbodd3
243600050421      ** Fattura inizio mese
243700050421     c     fatfiv        wheneq    §qcfiv
243800050421     c                   except    tbodi3
243900050421      ** Fattura fine mese
244000050421     c                   other
244100050421     c                   except    tbodf3
244200050421     c                   endsl
244300050421
244400050421
244500050421     c                   ENDSR
244600050404      *****************************************************
244700050404      ** Stampa totale fattura e contabilizza
244800050404      *****************************************************
244900050404     c     Totcod        begsr
245000050404      *
245100050404      * diritto di fatturazione
245200050404      * se deve essere ancora stampato lo faccio ora
245201161024      * ASTERISSCO TUTTO IN QUANTO VIENE STAMPATO SOTTO LA BOLLA SULLA QUALE SI
245202161024      * E' CALCOLATO
245300161024     c****               if        stp_vard = ' '
245400050404      ** salto pagina
245500161024     c****               exsr      salpag
245600030923      *
245700161024     c****               select
245800030923      ** autofattura
245900161024     c**** fattft        wheneq    1
246000161024     c****               except    ddvdd2
246100050404      ** Fattura inizio mese
246200161024     c**** fatfiv        wheneq    §qcfiv
246300161024     c****               except    ddvdi2
246400050404      ** Fattura fine mese
246500161024     c***                other
246600161024     c***                except    ddvdf2
246700161024     c***                endsl
246800030923      *
246900161024     c***                eval      stp_vard = 'N'
247000161024     c***                endif
247100050427      * se c'è fatturazione raggruppata eseguo il totale dell'ultimo codice di bollettazione
247200050427     c                   if        tasfrgpre =  'S'
247300050421     c                   exsr      SR_totbol
247400050421      * se ci sono più di un codice di bollettazine raggruppato faccio anche il riepilogo
247500050421     c                   If        itot > 1
247600050421     c                   exsr      SR_riepil
247700050421     c                   endif
247800050421      * pulisco la schiera dei subtotali per raggruppamento
247900050421     c                   clear                   sktot
248000050426     c                   clear                   itot
248100050421
248200050421     c                   endif
248300050421
248400050404      * se ci sono degli importi con decimali < 0,03 --> porto a 0,03
248500050404      *  per avere il minimo del calcolo dell'iva
248600050405     c                   do        5             ww                2 0
248700050405     c                   if        imdw(ww)>0 and imdw(ww)<0,03
248800050405     c                   eval      imdw(ww)=0,03
248900050405     c                   eval      v1d(ww) =0,03
249000050405     c                   endif
249100050405     c                   enddo
249200050404     c
249300050404      *
249400050404      ** Esegue Calcolo IVA
249500050407     c                   Exsr      Caliva
249600050404      *
249700050405      *
249800050405      * 11 ON - stampo dicitura : bollo assoloto virtualmente...
249900050405     c                   eval      *in11='0'
250000050405     c                   If         impbold > 0
250100050405     c                   z-add     0             totbold
250200050405      ** calcolo bollo esenzione
250300050405     c                   exsr      calbol
250400050415     c                   add(h)    totbold       fatbold
250500050405     c                   endif
250600050405      ** memorizza importi bolli in contropartita
250700050405     c                   IF        fatbold > 0
250800050405     c                   eval      *in11='1'
250900050405     c                   movel     'VARIE   '    Kcc
251000050405     c                   move      '6'           Kcc
251100050415     c                   z-add     fatbold       wimpor
251200050405     c                   exsr      conric
251300050405     c                   exsr      conmem
251400050405     c                   Endif
251500050405      *
251600050405      *------------------------------------------------------------------
251700050405     c                   z-add     0             fattotd
251800050405      * sposto  gli importi con 3 decimali in un campo con 2 decimali
251900050405     c                   do        5             a
252000050405     c                   add(h)    imdw(a)       imd(a)
252100050405     c                   end
252200050405     c
252300050405      * Se importi con decimali e c'e' una solo controp.<0,03 --> la porto
252400050405      *  a 0,03
252500050405     c                   clear                   wconta
252600050405     c
252700050405     c                   xfoot     icpdw         wtoticpd
252800050405     c
252900050405    1c                   if        wtoticpd<0,03
253000050405
253100050405    2c                   do        50            a
253200050405    3c                   if        icpdw(a)>0
253300050405     c                   add       1             wconta
253400050405    3c                   endif
253500050405    2c                   enddo
253600050405
253700050405    2c                   if        wconta=1
253800050405
253900050405    3c                   do        50            a
254000050405    4c                   if        icpdw(a)>0
254100050405     c                   z-add     0,03          icpdw(a)
254200050405    4c                   endif
254300050405    3c                   enddo
254400050405    2c                   endif
254500050405    1c                   endif
254600050405     c
254700050405     c
254800050405    1c                   do        50            a
254900050405     c                   add(h)    icpdw(a)      icpd(a)
255000050405    1c                   enddo
255100050405
255200050405      ** Calcolo Totale Fattura
255300050405
255400050405     c                   do        5             a
255500050405     c                   add       imd(a)        fattotd
255600050405     c                   add       ivd(a)        fattotd
255700050405     c                   enddo
255800050405
255900050405     c                   add(h)    fatbold       fattotd
256000050405      **
256100050405      *------------------------------------------------------------------
256200050405      ** Stampo totale fattura
256300050405     c                   exsr      wtrtot
256400050405      *------------------------------------------------------------------
256500050405      ** Imputazione contabile per le sole fatture (escludo le distinte)
256600050405     c                   If        Fattft = 0
256700050405     c                   exsr      crepn
256800050405     c                   endif
256900050405      *------------------------------------------------------------------
257000050405      ** Elenco distinte spedizione
257100050405     c                   If        fattft = 1
257200050405     c                   exsr      wtrdis
257300050405     c                   endif
257400060214      *------------------------------------------------------------------
257500060214      ** Scrittura riepilogo tipo invio fattura
257600060214     c                   exsr      wrtift
257601170127      ** se dichiarazione intento registro il progressivo fatturato nel file di lavoro
257602170127      ** che utilizzo per il controllo del superamento del plafon dichiarato
257603170127     c                   If        Dicesito = 'S'
257604170127     c                   update    wfdicf00
257605170127     c                   Endif
257700050405
257800050405     c                   clear                   wanspe
257900050405      *------------------------------------------------------------------
258000050509      * aggiorna cliente di bollettazione se non c'è raggruppamento
258100050509     c                   if        tasfrgpre =  ' '
258200050509      * recupero il codice di raggruppamento/bollettazione
258300050509     c                   if        savtasksc <> *blank
258400050509     c                   move      savtasksc     clnksc
258500050509     c                   endif
258600050509
258700050509     c     Keybac        chain     ybacl01l
258800050509
258900050405     c                   exsr      aggbcl
259000050509     c                   endif
259100050405      *------------------------------------------------------------------
259200050405      ** pulisco i campi di lavoro
259300050405     c                   exsr      blank
259400050405
259500050405     c                   Endsr
259600050405
259700050405      *******************************************************
259800050405      * Aggiorna Data ultima e prima spedizione
259900050405      *******************************************************
260000050405     c     aggbcl        begsr
260100050506
260200050405
260300050405      ** Data Ultima Spedizione
260400050405     c                   if        dspult > acldus
260500050405     c                   move      dspult        acldus
260600050405     c                   endif
260700050405
260800050405      ** Data prima spedizione
260900050405     c                   if        acldps = 0 or dsppri < acldps
261000050405     c                   move      dsppri        acldps
261100050405     c                   end
261200050405
261300050405     c                   Update    ybacl000
261400050509
261500050509      ** aggancio anche cnclp00f per aggiornare anche ORION senza trigger
261600050509
261700050509     c                   move      aclkcc        clpkcc
261800050509     c                   move      aclksc        clpksc
261900050509
262000060220     c     keyclp        Chain(e)  cnclp00f
262100060220    1c                   if        %error
262200060220     c     keyclp        Chain(n)  cnclp00f
262300060220     c                   Clear                   Trul82ds
262400060220     c                   Eval      ul82§rrn = cnclpnrri
262500060220     c                   Eval      UL82§FIL = 'CNCLP00F'
262600060220      * Effettuo la chiamata al *PGM d utilità
262700060220     c                   Call(e)   'TRUL82R'
262800060220     c                   Parm                    Trul82ds
262900060220      * se alloccato recupero utente e programma e preaparo il msg da inviare
263000060220    2c                   If        UL82§sts = 'A'
263100060220     c                   eval      msgjob = 'Il lavoro ' + %trim(UL82§JNAM) +
263200060220     c                             ',Utente ' + %trim(UL82§JUSR) + ' sta -
263300060220     c                             bloccando la FATTURAZIONE : TERMINARE'
263400060220     c                   Clear                   Trul82ds
263500060220     c                   Eval      ul82§rrn = cnclpnrri
263600060220     c                   Eval      UL82§FIL = 'CNCLP00F'
263700060220     c                   Eval      UL82§F5  = 'S'
263800060220     c                   Eval      UL82§F7  = 'S'
263900060220     c                   Eval      UL82§mss = msgjob
264000060220     c                   Eval      UL82§num = 99
264100060221     c                   Eval      UL82§att = 60
264200060220      * Effettuo la chiamata al *PGM d utilità
264300060220     c                   Call(e)   'TRUL82R'
264400060220     c                   Parm                    Trul82ds
264500060220    2c                   endif
264600060220
264700060220     c     keyclp        Chain     cnclp00f
264800060220
264900060220    1c                   endif
265000060220
265100050509     c                   If        %found(cnclp00f)
265200050509     c                   move      acldps        clpdps
265300050509     c                   move      acldus        clpdus
265400050509     c                   update    cnclp000
265500050509     c                   endif
265600050405
265700050509     c                   clear                   dspult
265800050509     c                   clear                   dsppri
265900050509
266000050405     c                   endsr
266100050411      ********************************************************
266200050411      ** Stampa testata e dettaglio e memorizzo dati  X CTB
266300050411      ********************************************************
266400050411     c     stacod        Begsr
266500050412      * memorizzo se devo stampare la dicitura del calcolo del peso in fondo alla fattura
266600150929     c                   If        Wf_tasspc = 'S' or Wf_tasspc = 'D'
266700050411     c                   eval      wcalcevdl = 'S'
266800150911     c                   Endif
266900150911      * mi salvo il peso da fatturare per stamparlo in fattura se vdl o desunto
267000150911     c                   If        Wf_tasspc = 'S' or Wf_tasspc = 'D'
267100050411     c                   eval      stapkf=wf_TaspkcN
267200050411     c                   else
267300050411     c                   eval      stapkf=taspkf
267400050411     c                   endif
267500090915      * memorizzo i colli originali
267600090915     c                   eval      stpncl = Wf_tasncl
267601160428      * mi salvo il volume da stampare
267602160428     c                   eval      stavlf = wf_tasvlfd
267609160426
267700050411      * verifico se è stata stampata la testata
267800050411    1c                   If        Wstcod = 0
267900050411      ** AUTOFATTURA
268000050411    2c                   IF        Fattft = 1
268100050411     c                   z-add     999999        fatnft
268200120214     c                   z-add     §qcfiv        fatfiv
268300120214    4c                   if        *in20                                        FINE MESE
268400120214     c                   z-add     dtdfi8        fatdft
268500120214     c                   move      viddfi        dataeur
268600120214     c     *dmy          move      dataeur       stadft
268700120214     c                   else                                                   NO FINE MESE
268800120214     c                   z-add     dtdft8        fatdft
268900120214     c                   move      viddft        dataeur
269000120214     c     *dmy          move      dataeur       stadft
269100120214    4c                   endif
269200050411   X2c                   else
269300050411      ** Fattura Fine Mese
269400050411    3c                   If        Fatifm = 'F' and *in20                                FINE MESE
269500050411     c                   add       1             nftfm
269600050411     c                   z-add     nftfm         fatnft
269700050411      * memorizzo il primo numero fattura fine mese
269800050426    4c                   If        Wfine =  ' '
269900050411     c                   z-add     nftfm         wfipri
270000050411     c                   movel     '1'           wfine
270100050411    4c                   endif
270200050411     c                   z-add     dtdff8        fatdft
270300050411     c                   z-add     §qcfi2        fatfiv
270400050411     c                   move      viddff        dataeur
270500050411     c     *dmy          move      dataeur       stadft
270600050411   X3c                   else                                                   INIZIO MESE
270700050411      ** Fattura inizio mese
270800050411     c                   add       1             nftim
270900050411     c                   z-add     nftim         fatnft
271000050411      * memorizzo il primo numero fattura inizio mese
271100050411    4c                   if        winiz = ' '
271200050411     c                   z-add     nftim         winpri
271300050411     c                   movel     '1'           winiz
271400050411    4c                   endif
271500050411      *
271600050411     c                   z-add     §qcfiv        fatfiv
271700050411      * fatturazione mensile
271800050411    4c                   if        *in20                                        FINE MESE
271900050411     c                   z-add     dtdfi8        fatdft
272000050411     c                   move      viddfi        dataeur
272100050411     c     *dmy          move      dataeur       stadft
272200050411     c                   else                                                   NO FINE MESE
272300050411     c                   z-add     dtdft8        fatdft
272400050411     c                   move      viddft        dataeur
272500050411     c     *dmy          move      dataeur       stadft
272600050411    4c                   endif
272700050411    3c                   end
272800050411    2c                   end
272900050411      **
273000050411     c                   movel     fatksc        filpag
273100050411      * decodfica inn Azorg
273200050411     c                   exsr      organi
273300050411      ** stampa testata fattura
273400050411     c                   exsr      wtrhdg
273500050411      ** valorizzo il flag di stampa testata
273600050411     c                   z-add     1             wstcod
273700050411    1c                   endif
273800050411
273900050421      * verifico se devo stampare anagrafica bollettazione
274000050421    1c                   If        Wstbsc = 'Y'
274100050421      ** stampa testata fattura
274200050421     c                   exsr      wtrbsc
274300050421      ** valorizzo il flag di stampa testata
274400050421     c                   clear                   wstbsc
274500050421    1c                   endif
274600050411      ** Valorizzo i campi del TITAS
274700050411
274800050411     c                   movel     '1'           tasfts
274900050411     c                   movel     '1'           tasfic
275000050411     c                   z-add     fatnft        tasnft
275100050411     c                   z-add     fatdft        tasdft
275200050411     c                   z-add     fatfiv        tasfiv
275300050421
275400050421      ** contabilizzo
275500050421     c                   Exsr      Con001
275600050411      ** stampa fattura dettaglio spediixone
275700050411     c                   Exsr      Wtrdet
275701170127      ** aggiorno progressivo fatturato solo se presente dichiarazione intento
275702170127     c                   if        dicesito = 'S' and tasfei = ' '
275703170127     c                   add       tasimv        dicprgfat
275704170127     c                   endif
275800050421      ** se bolla raggruppata mi salvo i valori in schiera
275900050427     c                   If        wf_tasfrg = 'S'
276000050421     c                   if        Tipbol = 'F'
276100050421     c                   add       tasimv        tfra
276200050421     c                   else
276300050421     c                   add       tasimv        tass
276400050421     c                   endif
276500050421     c                   add       tasimv        timp
276600050421     c                   endif
276700050411
276800050411      * aggiorno titas
276900050429     c                   update    Titas30
277000050429      *
277100050412     c                   exsr      aggbol
277200050412C     * imposto ultima e prima data spedizione per codice raggruppamento
277300050412     c                   if        tasdsp > dspult
277400050412     c                   z-add     tasdsp        dspult
277500050412     c                   endif
277600050412
277700050412     c                   if        dsppri = 0 or tasdsp < dsppri
277800050412     c                   z-add     tasdsp        dsppri
277900050412     c                   endif
278000050412
278100050412C     *
278200050412     c                   endsr
278300050412      *****************************************************
278400050412      ** Stampa testata fattura
278500050412      *****************************************************
278600050412     c     wtrhdg        Begsr
278700050412
278800050412     c                   z-add     0             page              5 0
278900050412      * numero righe di stampa dettaglio in prima pagina
279000050412     c                   z-add     50            wrighe
279100050412      * Pulizia scheire della legenda
279200050412     c                   movel     *blanks       fes
279300050412     c                   movel     *blanks       nes
279400050412     c                   movel     *blanks       les
279500050412     c                   movel     *blanks       led
279600050412     c                   movel     *blanks       lin
279700051110
279800051110     c                   clear                   wmail
279900051118     c                   clear                   indmail
280000051110
280100050412     c                   movel     '001'         comlsf
280200050412     c                   movel     'L'           spwfls
280300050412      * aggancio FNSPE prima con il codice di raggruppamento  se manca con il codice di fatturazione
280400050412      *
280500051110    1c                   if        savtasksc <> *blank
280600050412     c                   move      savtasksc     Wclpksc                        * cod.raggruppamento
280700051118
280800051118     c                   exsr      RIC_fnspe
280900051110      * se non ho trovato ne l'indirizzo mail che l'etichetta
281000051110    2c                   If        wmail <> 'S' and wanspe <> 'S'
281100051110      * cerco con il codice di fatturazione
281200051110     c                   move      clnksc        wclpksc                        * cod fatturazione
281300051118     c                   exsr      RIC_fnspe
281400051110      *
281500051110    2c                   endif
281600051110      *
281700051110  x 1c                   else
281800050412     c                   move      clnksc        wclpksc                        * cod fatturazione
281900051118     c                   exsr      RIC_fnspe
282000051110    1c                   endif
282100051110      * se c'è indirizzo di spedizione e non c'è indirizzo mail stampo l'etichetta e la fattura
282200051110     c                   If        wanspe = 'S' and wmail <> 'S'
282300050412     c                   movel     '§*§'         wlbeti
282400051110     c                   endif
282500050412
282600050412      ** stampa etichetta per invio fattura
282700050412     c                   If        fatsta <> *blanks
282800050412     c                   clear                   ds15
282900050412     c                   move      '15'          cod
283000050412     c                   movel     *blanks       key
283100050412     c                   movel     fatsta        key
283200050412     c     KTabc         chain     tabel                              30
283300050412     c  N30              movel     tbluni        ds15
283400051125      ** se stato non italia e non esiste la mail stampo etichetta
283500051125     c                   if        §15ita <> 'I' and not *in30 and wmail = ' '
283600050412     c                   movel     '§*§'         wlbeti
283700050412     c                   endif
283800050412     c                   endif
283900051110      ** mail
284000051110     c                   If        wmail = 'S'
284100060324
284200060324      * cerco indirizzo mail mittente in base al P.O. di appartenenza del codice di
284300060324      * BOLLETTAZIONE  SE NON TROVATO NON MANDO VIA MAIL
284301161017      *
284302161017      * 17/10/16
284303161017      *
284304161017      * (Bocchi/Delledonne) visto che la mail del mittente è stata cambiata da INFO a AMM
284305161017      * e che il nuovo OTTICO DOCFLOW non permette di visualizzare le fatture sia dalla
284306161017      * filiale del codice di bollettazione che da quello di fatturazione cambiamo
284307161017      * l'indirizzo MITTENTE mail della fattura dall'AMM della filiale del codice di
284308161024      * BOLLETTAZIONE all'AMM della filiale contabile del codice di FATTURAZIONE
284400161017
284500161017     c* 17/10/16 *       movel     bolksc        kscfil
284501170111     c* 11/01/17 *       movel     ASFAT         kscfil
284502170111     c                   movel     fatksc        kscfil
284600060324     c                   clear                   trul44ds
284700060324     c                   eval      d44pgm = 'TNSF07R'
284800060324     c                   eval      d44po  = Kscfil
284900060324     c                   call      'TRUL44R'
285000060324     c                   parm                    trul44ds
285100060328      * se non trovato
285200060328     c                   If        d44mit = *blanks
285300060328     c                   eval      d44mit = constmail
285400060328     c                   endif
285500060324      * stampo indirizzo mittente
285600060324     c                   eval      mittmail = 'E-mail: ' + %trim(d44mit) + ';'
285700060324
285800060324     c                   eval      stmail1 = 'EMAIL1:'+ %subst(indmail: 1: 38)
285900061009     c                   eval      stmail2 = 'EMAIL2:'+ %subst(indmail: 39: 23)
286000060324     c                   endif
286100060324
286200060324     c                   If        wmail = ' '
286300051110     c                   clear                   stmail1
286400051110     c                   clear                   stmail2
286500060324     c                   clear                   mittmail
286600051110     c                   endif
286700050412      ** Autofattura
286800050412    1c                   if        fattft = 1
286900050412      *
287000050412     c                   if        not *in37
287100050412
287200050412     c                   movel     'DIS.SPE'     kmdst
287300050412     c                   move      kcdsi         kmdst
287400050412     c                   open      ftwcodds
287500050412     c                   seton                                        37
287600050412
287700050412     c                   endif
287800050412      * verifico quanti records ho stampato
287900050412     c                   if        nrd > 0
288000050412     c                   eval      §nrfd = (cur_page * 74)
288100050412     c                   if        §nrfd >= 600000
288200050412     c                   close     ftwcodds
288300050412     c                   open      ftwcodds
288400050412     c                   endif
288500050412     c                   endif
288600050412      *
288700050412     c                   except    hcodd
288800050412     c                   z-add     0             nrd
288900050412      * se codice bolletazione diverso da codice fatturazione
289000050421      * e non c'è codice raggruppamento
289100050412     c                   if        savtasksc <> ktasksc and savtasksc <> *blanks
289200050421     c                             and wf_tasfrg = ' '
289300050412     c                   except    h2codd
289400050412     c                   add       2             nrd
289500050412     c                   endif
289600050412      *
289700050412   x1c                   else
289800050412
289900050412      ** fattura inizio mese
290000050412
290100050412    2c                   if        fatfiv = §qcfiv
290200050412
290300050412     c                   if        not *in35
290400050412     c                   movel     'FT.C.I.'     kmdst
290500050412     c                   move      kcdsi         kmdst
290600050412     c                   open      ftwcodim
290700050412     c                   seton                                        35
290800050412     c                   endif
290900050412
291000050412     c                   if        *in35
291100000414      * sommo la fattura solo se non va stampata in sede
291200050412     c                   if        wlbeti = *blanks
291300050412     c                   add       1             §NRFI
291400050412     c                   endif
291500050412     c                   if        §nrfi > 1100
291600050412     c                   close     FTWCODIM
291700050412     c                   open      FTWCODIM
291800050412     c                   z-add     1             §NRFI
291900050412     c                   endif
292000050412     c                   endif
292100050412
292200050412     c                   except    hcodi
292300050412     c                   z-add     0             nri
292400050412      * se codice bolletazione diverso da codice fatturazione
292500050421      * e non c'è codice raggruppamento
292600050412     c                   if        savtasksc <> ktasksc and savtasksc <> *blanks
292700050421     c                             and wf_tasfrg = ' '
292800050412     c                   except    h2codi
292900050412     c                   add       2             nri
293000050412     c                   endif
293100050412
293200050412   x2c                   else
293300050412
293400050412      ** Fattura fine mese
293500050412
293600050412     c                   if        not *in38
293700050412     c                   movel     'FT.C.F.'     kmdst
293800050412     c                   move      kcdsi         kmdst
293900050412     c                   open      ftwcodfm
294000050412     c                   seton                                        38
294100050412     c                   endif
294200050412
294300050412     c                   If        *in38
294400000414      * sommo la fattura solo se non va stampata in sede
294500050412     c                   if        wlbeti = *blanks
294600050412     c                   add       1             §NRFM
294700050412     c                   endif
294800050412     c                   if        §nrfm > 1100
294900050412     c                   close     FTWCODFM
295000050412     c                   open      FTWCODfM
295100050412     c                   z-add     1             §NRFM
295200050412     c                   endif
295300050412     c                   endif
295400050412
295500050412     c                   except    hcodf
295600050412     c                   z-add     0             nrf
295700050412      * se codice bolletazione diverso da codice fatturazione
295800050421      * e non c'è codice raggruppamento
295900050412     c                   if        savtasksc <> ktasksc and savtasksc <> *blanks
296000050421     c                             and wf_tasfrg = ' '
296100050412     c                   except    h2codf
296200050412     c                   add       2             nrf
296300050412     c                   endif
296400050412
296500050412    2c                   endif
296600050412
296700050412    1c                   endif
296800050412
296900050412     c                   endsr
297000051118      *****************************************************************
297100051118      * Ricerca in FNSPE sia l'etichetta che la mail oppure nelle note
297200051118      *****************************************************************
297300051118     c     RIC_fnspe     begsr
297400051118
297500051118     c     kspe          chain     fnspe03l                           91
297600051118      *
297700051118    1c                   if        %found(fnspe03l)
297800051118      * memorizzo che c'e' Fnspe  se ragione sociale diversa da blank
297900051118    2c                   if        sperag <> *blanks
298000051118     c                   movel     'S'           wanspe
298100051118    2c                   endif
298200051118      * verifico se c'è indirizzo mail
298300051118     c     ksp2          chain     fnsp201l
298400051118    2c                   if        %found(fnsp201l) and SP2EST <> *BLANKS
298500051118     c                   eval      wmail = 'S'
298600060324     c                   eval      indmail = %trim(sp2est) + ';'
298700051118    2c                   endif
298800051118
298900051118    1c                   endif
299000051129      * se non trovato FNSPE  cerco nella nota "84"
299100051118
299200051129    1c                   If        not %found(fnspe03l)
299300051118     c                   eval      Knk1 = '0151' + %editc(WCLPksc:'X')
299400051118     c     Kntc          chain     Tfntc01l
299500051118    2c                   If        %found(tfntc01l) and ntcrnt <> *blanks
299600051118     c                   eval      wmail = 'S'
299700060324     c                   eval      indmail = %trim(ntcrnt) + ';'
299800051118    2c                   endif
299900051118    1c                   endif
300000051118
300100051118     c                   endsr
300200050421      ******************************************************
300300050421      * stampa dati anagrafici codice bollettazione raggruppato
300400050421      ******************************************************
300500050421     c     wtrbsc        begsr
300600050421
300700050421      ** salto pagina
300800050421     c                   exsr      salpag
300900050421
301000050421      * S T A M P A   : 1 riga anagrafica
301100050421
301200050421     c                   select
301300050421      * Autofattura
301400050421     c                   when      fattft = 1
301500050421     c                   except    db1odd
301600050421      * Fattura inizio mese
301700050421     c                   when      fatfiv = §qcfiv
301800050421     c                   except    db1odi
301900050421      * Fattura fine mese
302000050421     c                   other
302100050421     c                   except    db1odf
302200050421     c                   endsl
302300050421
302400050620      * valorizzo il primo numero pagina da
302500050620     c                   eval      tpgi = page
302600050421      ** salto pagina
302700050421     c                   exsr      salpag
302800050421
302900050421      * S T A M P A   : 2 riga anagrafica
303000050421
303100050421     c                   select
303200050421      * Autofattura
303300050421     c                   when      fattft = 1
303400050421     c                   except    db2odd
303500050421      * Fattura inizio mese
303600050421     c                   when      fatfiv = §qcfiv
303700050421     c                   except    db2odi
303800050421      * Fattura fine mese
303900050421     c                   other
304000050421     c                   except    db2odf
304100050421     c                   endsl
304200050421
304300050421     c                   endsr
304400050421
304500050412      ******************************************************
304600050412      * stampa dettaglio
304700050412      ******************************************************
304800050412     c     wtrdet        begsr
304900050412
305000050412      * prepara stampa dettaglio
305100050412
305200050412     c                   exsr      presta
305300050412     c                   clear                   wrma15
305400050412     c                   clear                   rma15
305500050412     c                   clear                   rma09
305600050412     c                   setoff                                       29
305700050412      * se    bolle di richiesta bolle firmate cerco il record su titaa
305800050412      * valorizzo il nominativo del destinatario da stampare se taarsc diverso da blanks
305900050412     c                   if        tascbo = 'FO' or tascbo = 'FF'
306000050412     c                             or  tascbo = 'FI'
306100050412     c                   eval      §trc = 'O'
306200050412     c     ktaa          chain     Titaa30c
306300050412     c                   if        %found(titaa30c)  and
306400050412     c                             taarsc <> *blanks
306500050412     c                   eval      rsc13 = taarsc
306600050412     c                   endif
306700030512      * cerco il riferimento mittente alfanumerico
306800050412     c                   eval      §4trc = 'A'
306900050412     c     kta4          chain     tita430c
307000050412     c                   If        %found(tita430c)
307100050412     c                   movel     ta4not        dta4a
307200050412     c                   else
307300050412     c                   clear                   dta4a
307400050412     c                   endif
307500050412
307600050412     c                   If        §ta4arma <> *blanks
307700050412      * TA4Arma è alfanumerico di 15 se devono prendere
307800050412      * si devono spostare a destra e prendere gli ultimi 9 caratteri
307900050412      * e metterli in un campo di 9 alfanumerico che prenderà il posto
308000050412      * in caso di bolle firmate di Rmn9
308100050412     c                   movel     §ta4aRMA      Rma15
308200050412      * tolgo i blank non significativi
308300050412     c                   eval      II = 1
308400050412     c                   dow       II <= %len(%trim(Rma15))
308500050412      *
308600050412     c                   If        %subst(%trim(Rma15):II:1) <> *blank
308700050412     c                   eval      WRma15 = (%trim(Wrma15)
308800050412     c                                      + %subst(%trim(Rma15):II:1))
308900050412     c                   endif
309000050412      *
309100050412     c                   eval      II = II + 1
309200050412      *
309300030512     c                   enddo
309400050412     c                   evalr     rma09 = %trim(Wrma15)
309500050412     c                   seton                                        29
309600050412      *
309700050412     c                   endif
309800050412      *
309900050412     c                   endif
310000050412
310100050412      ** Carico schiera sigle varie X stampa legenda
310200050412    1C                   do        20            a
310300050412     c                   clear                   varfed
310400050412      * se varia valorizzata
310500050412    2C                   If        S(a) <> *blanks and id(a) > 0
310600050412      * se si tratta di bolla fedex controllo se la varia ha descrizione div
310700160315    3c                   if        Wf_Tasfie='F'
310800050412     c                   movel     'VARIE   '    KCC
310900050412     c                   move      S(A)          KCC
311000050412     c                   z-add     1             A2
311100050412     c     kcc           Lookup    cc(A2)                                 05
311200020320    4c                   if        *in05
311300050412     c     a2            occur     dscc
311400050412      * descrizione  a parte memorizzo in altra schiera
311500050412    5c                   if        §ccdfe='S'
311600050412     c                   eval      varfed='S'
311700050412     c                   z-add     1             A2
311800050412     c     s(a)          lookup    fes(a2)                                05
311900050412      ** non trovata in legenda la carico
312000050412    6c                   if        not *in05
312100050412     c                   z-add     1             a2
312200050412     c     *blanks       lookup    fes(a2)                                06
312300050412     c   06              movel     s(a)          fes(a2)
312400050412    6c                   endif
312500050412    5c                   endif
312600050412    4c                   endif
312700050412    3c                   endif
312800050412
312900020320    3c                   if        varfed=*blanks
313000050412     c                   z-add     1             A2
313100050412     c     s(a)          lookup    nes(a2)                                05
313200050412      ** non trovato in legenda la carico
313300050412    4c                   if        not *in05
313400050412     c                   z-add     1             a2
313500050412     c     *Blanks       lookup    nes(a2)                                06
313600050412     c   06              movel     s(a)          nes(a2)
313700050412    4c                   endif
313800050412      *
313900050412    3c                   endif
314000050412    2c                   endif
314100050412    1c                   enddo
314200030924      ** se devo ancora stampare la varia D e l'ho già calcolata la tolgo dalle varie
314300050412      ** da stampare  e la memorizzo per stamparla al momento giusto
314400160928     c                   if        stp_vard = ' ' and cal_vard = 'N'
314500160928     c                   z-add     1             dd                3 0
314600160928     c     'D'           lookup    S(dd)                                  13
314700160928     c                   if        %found
314800160928     c***                z-add     id(dd)        variadd
314900160928     c***                z-add     0             id(dd)
315000160928     c***                clear                   s(dd)
315001160928      * non stampo più la varia D in seconda pagina o in fondo alla fattura ma legata
315002160928      * alla spedizine dove ho calcolato
315003160928     c                   eval      stp_vard = 'N'
315100160928     c                   endif
315200050412      *
315300160928     c                   endif
315400050412      *
315500050412      ** salto pagina
315600050412     c                   exsr      salpag
315700050412
315800050412      * S T A M P A   : 1 riga dettaglio
315900050412
316000050412     c                   select
316100050412      * Autofattura
316200050412     c                   when      fattft = 1
316300050412     c                   except    dcodd
316400050412      * sommo numero spedizioni per autofatture da stampare di fianco al totale nella
316500050412      * lista delle distinte di spedizione
316600050412     c                   add       1             numspe
316700050412      * Fattura inizio mese
316800050412     c                   when      fatfiv = §qcfiv
316900050412     c                   except    dcodi
317000050412      * Fattura fine mese
317100050412     c                   other
317200050412     c                   except    dcodf
317300050412     c                   endsl
317400050412
317500050412      * se devo stampare il riferimento mittente recupero da TAA
317600050412
317700000119     c                   clear                   wmitori
317800050412     c                   clear                   wparcel
317900110711     c                   clear                   stpcup
318000050412     c                   setoff                                       192223
318100080506     c                   setoff                                       2428
318200050412
318300050412     c                   If        stprmo <> *blank or
318400050412      * oppure se bolla di recupero dei ritiri annullati
318500050412     c                             tascbo='FY'
318600050412     c                   eval      §trc = 'O'
318700050412     c     ktaa          chain     Titaa30c
318800050412     c                   if        %found(titaa30c)
318900050412     c                   movel     taarsc        wmitori
319000050412     c                   endif
319100050412      *
319200050412     c                   endif
319300050412
319400050412      * Se bolla di recupero ritiri annullati, stampo anche il rif.alfabetic
319500050412     c                   If        tascbo='FY'
319600050412     c                   eval      §4trc = 'A'
319700050412     c     Kta4          chain     tita430c
319800050412     c                   If        %found(tita430c)
319900050412     c                   movel     ta4not        dta4a
320000050412     c                   else
320100050412     c                   clear                   dta4a
320200051003     c                   endif
320300050412     c                   movel     §ta4arma      Wmitalf
320400050412     c                   seton                                        22
320500050412     c                   endif
320600050412
320700050412
320800050412      * stampa parcel perchè DPD e non è distinta di spedizioni
320900050412
321000050412     c                   if        stppar <> *blank and Fattft <> 1
321100060703      **** SOSTITUITO CON DSBL4I ************************                    *
321200060703      * se linea di partenza è dpd prelievo dal riferimento numerico         *
321300060703     c****               if        lnpdpd = 'S'                              *
321400060703     c****               move      tasrmn        com12                       *
321500060703     c****               eval      wparcel = com12                           *
321600060703      * se linea di partenza non è DPD ricerco il parcel nel file tita4      *
321700060703     c****               else                                                *
321800060703      *                                                                      *
321900060703     c****               eval      §4trc = 'F'                               *
322000060703     c**** Kta4          chain     tita430c                                  *
322100060703     c****               If        %found(tita430c)                          *
322200060703     c****               movel     ta4not        dsbl4f                      *
322300060703     c****               eval      wparcel = §b4fnp                          *
322400060703     c****               endif                                               *
322500060703     c****               endif                                               *
322600060703      **** SOSTITUITO CON DSBL4I ************************                    *
322700050412
322800060703     c                   eval      §4trc = 'I'
322900060703     c     Kta4          chain     tita430c
323000060703     c                   If        %found(tita430c)
323100060703     c                   movel     ta4not        dsbl4i
323200060703     c                   eval      wparcel = §b4ipn
323300060703     c                   endif
323400050412     c                   endif
323500050412
323600060724      * se non è stato valorizzato nel mittente originale e neppure il parcel
323700060724      * verifico se si tratta di bolla fuori misura da addebitare a cliente DPD
323800060724      * riconosco un cliente dpd se stppar è diverso da blank (stampa numero parcel)
323900060724      * in più la bolla deve essere F3 codice bolla "B "
324000060724      * riconosco un fuori misura se nel tipo record O di titaa c'è la scritta OVERWEIGHT
324100060724     c                   If        (Wmitori = *blanks and wparcel = *blanks)
324200060724     c                             and stppar <> ' ' and tastbl = 'F3' and
324300060724     c                             tascbo = 'B '
324400060724     c                   eval      §trc = 'O'
324500060724     c     ktaa          chain     Titaa30c
324600060724     c                   if        %found(titaa30c) and
324700060724     c                             %subst(taarsc:1:10)= 'OVERWEIGHT'
324800060724     c
324900060724     c                   eval      %subst(wmitori:15:11) = 'OVERWEIGHT '
325000060724      * recupero numero parcel
325100071016     c****               eval      §4trc = 'A'
325200071016     c                   eval      §4trc = 'E'
325300060724     c     Kta4          chain     tita430c
325400060724     c                   If        %found(tita430c)
325500071016     c***                movel     ta4not        dta4a
325600071016     c                   movel     ta4not        dsbl4e
325700060724     c                   else
325800071016     c***                clear                   dta4a
325900071016     c                   clear                   dsbl4e
326000060724     c                   endif
326100071016     c***                movel     §ta4arpt      Wparcel
326200071016     c                   movel(p)  §b4erp        Wparcel
326300060724
326400060724     c                   endif
326500060724
326600060724     c                   endif
326700080512      * Se Cliente dpd  stppar = 'S'  e non è una distinta di spedizione
326800080505      * in caso di porto assegnato export con linea di arrivo DPD e bolla con ORM
326900080512      * recupero il numero ORM della dpd da stampare nella seconda riga solo
327000080512      * se i primi tre caratteri sono uguali a DPD
327100080512
327200080509    1c                   If        Tipbol ='A' and stppar = 'S'                 * Assegnato / DPD
327300080512     c                             and fattft <>  1                             * no autofattura
327400080505      * verifico la linea di arrivo
327500080505     c     taslna        chain     azorg01l
327600080506    2c                   If        %found(azorg01l)
327700080505     c                   movel     orgde3        og143
327800080506    3c                   if        §ogdpd = 'S'                                 * linea arrivo DPD
327900080505      * verifico se spedizione con ORM
328000080505     c                   eval      §4trc = 'M'
328100080505     c     Kta4          chain     tita430c
328200080506    4c                   If        %found(tita430c) and ta4not <> *blank        * ORM
328300080505      * recupero numero ORM DPD
328400080505     c                   eval      §4trc = 'A'
328500080505     c     Kta4          chain     tita430c
328600080506    5c                   If        %found(tita430c)                             * num. ORM DPD
328700080505     c                   movel     ta4not        dta4a
328800080506    6c                   If        §ta4arma <> *blanks
328900080506     c                   seton                                        28
329000080509      * imposto wmitori con il numero ORM DPD
329100080509      * pulisco il campo anche se ho il mittente originale valorizato
329200080509      * perchè non dobbiamo stampare + di 2 righe per spedizioni e preferiamo
329300080509      * stampare il numero dell'orm e non il mittente originale
329400080512     c                   if        %subst(§ta4arma :1:3) = 'DPD'
329500080512     c                   clear                   wmitori
329600080509     c                   eval      %subst(wmitori:1:5) = 'C.R. '
329700080509     c                   eval      %subst(wmitori:6:15) =  %trim(§ta4arma)
329800080512    6c                   endif
329900080506    6c                   endif
330000080506      *
330100080506    5c                   endif
330200080505      *
330300080506    4c                   endif
330400080506    3c                   endif
330500080506    2c                   endif
330600080506    1c                   endif
330700050412      * se non ci sono varie da stampare stampo solo il riferimento mittente se valorizzato
330800080505      * o se valorizzato il parcel o se devo stampare numero orm dpd
330900050412     c                   If        s(4) = *blank and ((wmitori <> *blank) or
331000050412     c                             (wparcel <> *blank))
331100050412
331200050412     c                   eval      *in19 = (wparcel <> *blanks)
331300050412
331400050412      ** salto pagina
331500050412     c                   exsr      salpag
331600050412
331700050412     c                   select
331800050412      ** autofattura
331900050412     c                   When      fattft = 1
332000050412     c                   except    dmiod2
332100050412      ** fattura inizio mese
332200050412     c                   When      fatfiv = §qcfiv
332300050412     c                   except    dmioi2
332400050412      ** fattura fine mese
332500050412     c                   other
332600050412     c                   except    dmiof2
332700050412     c                   endsl
332800050412
332900050412     c                   endif
333000050412      **
333100050412      * se ci sono ancora varie
333200050412     c                   z-add     4             a
333300050415     c                   dow       s(a) <> *blanks
333400050412     c                   movea     S(A)          S2
333500050412     c                   movea     ID(A)         I2D
333600050412      ** salto pagina
333700050412     c                   exsr      salpag
333800050412
333900050412     c                   select
334000050412      ** autofattura
334100050412     c                   When      fattft = 1
334200050412     c                   except    dcodd2
334300050412      ** fattura inizio mese
334400050412     c                   When      fatfiv = §qcfiv
334500050412     c                   except    dcodi2
334600050412      ** fattura fine mese
334700050412     c                   other
334800050412     c                   except    dcodf2
334900050412     c                   endsl
335000050412
335100050412     c                   movel     *blanks       wmitori
335200050412     c                   add       3             a
335300050412     c                   enddo
335400050412      *
335500050412      * se ci sono colli originali da stampare !!!
335600050412      *
335700050412    1c                   if        stpncl > 0
335800050412      ** Salto pagina
335900050412     c                   exsr      salpag
336000050412      *
336100050412     c                   select
336200050412      ** autofattura
336300050412     c                   When      fattft = 1
336400050412     c                   except    dcood2
336500050412      ** fattura inizio mese
336600050412     c                   When      fatfiv = §qcfiv
336700050412     c                   except    dcooi2
336800050412      ** fattura fine mese
336900050412     c                   other
337000050412     c                   except    dcoof2
337100050412     c                   endsl
337200050412
337300050412    1c                   endif
337400110711      * recupero codici cup-cig per la tracciabilità flussi
337500110711     c                   eval      §4trc = '4'
337600110711     c     Kta4          chain     tita430c
337700110711    1c                   If        %found(tita430c) and                         * codici Cup-CIG
337800110711     c                             ta4not <> *blanks
337900110711     c                   movel     ta4not        dta44
338000110711     c                   eval      StpCup = %trim(§ta4cup) + '-' +
338100110711     c                             %trim(§ta4cig)
338200110711    1c                   endif
338300110711      *
338400110711      *
338500110711    1c                   if        StpCup <> *blanks
338600110711      ** Salto pagina
338700110711     c                   exsr      salpag
338800110711      *
338900110711     c                   select
339000110711      ** autofattura
339100110711     c                   When      fattft = 1
339200110711     c                   except    dcupd2
339300110711      ** fattura inizio mese
339400110711     c                   When      fatfiv = §qcfiv
339500110711     c                   except    dcupi2
339600110711      ** fattura fine mese
339700110711     c                   other
339800110711     c                   except    dcupf2
339900110711     c                   endsl
340000110711
340100110711    1c                   endif
340200050412      *
340300050412      * se bolla con esenzione IVA stampo la desc. della tab. EI del codice esenzione
340400050412      *
340500050412     c                   if        tasfei <> ' '  and  desesiva <> *blank
340600030909      ** Salto pagina
340700050412     c                   exsr      salpag
340800050412      *
340900050412     c                   select
341000050412      ** autofattura
341100050412     c                   When      fattft = 1
341200050415     c                   except    ddeid2
341300050412      ** fattura inizio mese
341400050412     c                   When      fatfiv = §qcfiv
341500050415     c                   except    ddeii2
341600050412      ** fattura fine mese
341700050412     c                   other
341800050415     c                   except    ddeif2
341900050412     c                   endsl
342000050412
342100050412     c                   endif
342200030923      * se sono in seconda pagina della fattura e devo ancora stampare la varia D stampo
342300161024     c*****              if        stp_vard = ' ' and page = 2
342400050412      ** Salto pagina
342500161024     c*****              exsr      salpag
342600050412      *
342700161024     c*****              select
342800050412      ** autofattura
342900161024     c******             When      fattft = 1
343000161024     c*****              except    ddvdd2
343100050412      ** fattura inizio mese
343200161024     c******             When      fatfiv = §qcfiv
343300161024     c******             except    ddvdi2
343400050412      ** fattura fine mese
343500161024     c******             other
343600161024     c******             except    ddvdf2
343700161024     c******             endsl
343800050412
343900161024     c*****              eval      stp_vard = 'N'
344000050412
344100161024     c*****              endif
344200050412
344300050412      ** pulizia campi
344400050412     c                   z-add     0             mdsp
344500050412     c                   z-add     0             gdsp
344600050412     c                   movel     *blanks       cit3
344700050412     c                   movel     *blanks       s
344800050412     c                   z-add     0             id
344900050412     c                   movel     *blanks       s2
345000050412     c                   z-add     0             i2d
345100050412     c                   z-add     0             prtpord
345200050412     c                   movel     *blanks       rsc13
345300050412     c                   movel     *blanks       wmitori
345400050412     c                   z-add     0             rmn9
345500050412     c                   z-add     0             StpNcl
345600050412     c                   clear                   DesEsIVA
345700050412
345800050412     c                   endsr
345900050412      ******************************************************
346000050412      * Prepara i campi per la stampa dettaglio
346100050412      ******************************************************
346200050412     c     presta        begsr
346300050412
346400050412      ** data spedizione
346500050412     c                   move      tasmgs        gdsp
346600050412     c                   movel     tasmgs        mdsp
346700050412     c                   movel     *blanks       wtsp
346800050412      **
346900050412      ** tipo tariffa a valore o quantità
347000050412     c     tasctr        comp      400                                09  09
347100050412      ** tipi servizio da stampare
347200050412     c     tastsp        lookup    ts                                     05
347300050412     c   05              movel     tastsp        wtsp
347400050412
347500050412      *
347600050412      ** Sigla autom.filiale di partenmza
347700050412
347800050412     c                   if        taslnp <> savlnp
347900050412     c     taslnp        chain     azorg01l                           05
348000050412     c                   If        %found(azorg01l)
348100050412     c                   movel     orgpro        cit2
348200050412     c                   movel     orgfl1        lnpfl1
348300050412     c                   movel     orgde3        og143
348400050412     c                   eval      lnpdpd = §ogdpd
348500050412     c                   else
348600050412     c                   clear                   cit2
348700050412     c                   clear                   lnpfl1
348800050412     c                   clear                   lnpdpd
348900050412     c                   movel     taslnp        savlnp
349000050412     c                   endif
349100050412     c                   endif
349200050412      *
349300050412      ** Stampa cap
349400050412     c                   If        tipbol = 'F'
349500050412     c                   movel     tascad        wcap
349600050412     c                   else
349700050412     c                   movel     tascam        wcap
349800050412     c                   endif
349900050412      ** Provincia codice tassazione destino
350000050412     c                   z-add     1             a
350100050412     c     tascts        Lookup    cts(a)                                 05
350200050412     c   05A             occur     dsct
350300050412     c   05              movel     §ctprv        cit3
350400050412      *
350500050412     c                   z-add     taspor        prtpord
350600050412      ** caricamento varie per stampe
350700050412     c                   z-add     0             ID
350800050412     c                   movel     *BLANKS       S
350900050412     c                   movea     SV            S
351000050412     c                   z-add     VA            ID
351100050412      *
351200050412      ** MITT-DESTIN STAMPA
351300050412     c     tipbol        ifeq      'F'
351400050412     c                   movel     tasrsd        rsc13            13
351500050412     c                   else
351600050412     c     wccm          ifeq      8888
351700050412     c                   movel     taarsc        rsc13
351800050412     c                   else
351900050412     C                   CLEAR                   DSBS69
352000050412     C                   MOVEL     KNSIF         I69SIF
352100050412     C                   MOVEL     KCI           I69KCC
352200050412     C                   Z-ADD     TASCCM        I69KAC
352300050412     C                   CALL      'TIBS69R'
352400050412     C                   PARM                    DSBS69
352500050412     C                   PARM                    DSACO
352600050412     C                   PARM                    DSIND
352700050412     C                   PARM                    DSCLP
352800050412     C                   PARM                    DSCLS
352900050412     C     O69ERR        IFNE      ' '
353000050412     C                   CLEAR                   dsaco
353100050412     C                   endif
353200050412     C                   MOVEL     CACORAG       RSC13
353300050412     c                   end
353400050412     c                   end
353500050412      ** RIF.MITTENTE 8
353600960906     C                   Z-ADD     TASRMN        RMN9              9 0
353700050412      *
353800050412      * VOLUME
353900160427     C     STAVLF        COMP      0                                  4141
354000050412      * verificp se devo stampare il riferimento mittente
354100960906     C                   ENDSR
354200050413      ******************************************************
354300050413      * SALTO PAGINA
354400050413      ******************************************************
354500050413     c     SALPAG        BEGSR
354600050413     c                   SELECT
354700050413      ** AUTOFATTURA
354800050413    1c     FATTFT        WHENEQ    1
354900050413     c                   ADD       1             NRD
355000050413    2c     NRD           IFGT      WRIGHE
355100050413     c                   EXCEPT    RCODD
355200050413     c                   EXCEPT    HCODD2
355300050413     c                   Z-ADD     1             NRD
355400050413      * LA PRIMA PAGINA SONO 50 RIGHE, POI  74
355500050413     c                   Z-ADD     74            WRIGHE            3 0
355600050413    2c                   ENDIF
355700050413      ** FATTURA INIZIO MESE
355800050413    1c     FATFIV        WHENEQ    §QCFIV
355900050413     c                   ADD       1             NRI
356000050413    2c     NRI           IFGT      WRIGHE
356100050413     c                   EXCEPT    RCODI
356200050413     c                   EXCEPT    HCODI2
356300050413     c                   Z-ADD     1             NRI
356400050413     c                   Z-ADD     74            WRIGHE            3 0
356500050413    2c                   ENDIF
356600050413      ** FATTURA FINE MESE
356700050413   X1c                   OTHER
356800050413     c                   ADD       1             NRF
356900050413    2c     NRF           IFGT      WRIGHE
357000050413     c                   EXCEPT    RCODF
357100050413     c                   EXCEPT    HCODF2
357200050413     c                   Z-ADD     1             NRF
357300050413     c                   Z-ADD     74            WRIGHE            3 0
357400050413    2c                   ENDIF
357500050413      *
357600050413    1c                   ENDSL
357700050413     c                   ENDSR
357800050406      ********************************************************
357900050406      ** Stampa totale fattura codificati
358000050406      ********************************************************
358100050406     c     Wtrtot        begsr
358200050406
358300050406      * stampo legenda tutti netwrk tranne fedex
358400050406     c                   movea     nes           les
358500050406     c                   eval      varfed=' '
358600050406     c                   exsr      staleg
358700050406      * stampo legenda per fedex
358800050406     c                   movea     fes           les
358900050406     c                   clear                   led
359000050406     c                   clear                   lin
359100050406     c                   eval      varfed='S'
359200050406     c                   exsr      staleg
359300050406      *
359400050406      ** controllo se ci sono importi esenti iva in fattura
359500170202     c                   setoff                                       454647
359600050406     c                   do        5             a
359700050406
359800170127     c                   If        Imd(a) > 0 and ivd(a) = 0 and dicesito = 'S'
359900050406     c                   eval      *in45 = *on
360000050406     c                   endif
360100050406
360200050406     c                   enddo
360300050406      **
360400050406
360500050406      ** STAMPA PROT. ESENZIONE IVA ANAGRAFICA CLIENTE
360600050406
360700050406    1c                   if        *in45 or *in11
360800050406      * stampa dicitura della esenzione
360900110916     c                   eval      staese=cesenz
361000050406
361100050406     c                   SELECT
361200050406      ** autofattura
361300050406    2c     fattft        wheneq    1
361400050406     c                   if        *in45
361500050406     c  n11              add       2             nrd
361600050406     c   11              add       3             nrd
361601170202      * se la scritta dichiarazione intenti è oltre 113 caratteri
361602170202     c                   if        st_dicint2 <> *blank
361603170202     c                   add       1             nrd
361604170202     c                   seton                                        46
361605170202     c                   endif
361606170202      * se la scritta dichiarazione intenti è oltre 226 caratteri
361607170202     c                   if        st_dicint3 <> *blank
361608170202     c                   add       1             nrd
361609170202     c                   seton                                        47
361610170202     c                   endif
361700050406     c                   else
361800050406     c                   add       1             nrd
361900050406     c                   endif
362000050406      * controllo over
362100050406    3c                   if        nrd > wrighe
362200050406     c                   except    rcodd
362300050406     c                   except    hcodd2
362400050406
362500050406     c                   if        *in45
362600050406     c  n11              z-add     2             nrd
362700050406     c   11              z-add     3             nrd
362701170202      * se la scritta dichiarazione intenti è oltre 113 caratteri
362702170202     c                   if        st_dicint2 <> *blank
362703170202     c                   add       1             nrd
362704170202     c                   seton                                        46
362705170202     c                   endif
362706170202      * se la scritta dichiarazione intenti è oltre 226 caratteri
362707170202     c                   if        st_dicint3 <> *blank
362708170202     c                   add       1             nrd
362709170202     c                   seton                                        47
362710170202     c                   endif
362800050406     c                   else
362900050406     c                   z-add     1             nrd
363000050406     c                   endif
363100050406     c                   z-add     74            wrighe
363200050406    3c                   endif
363300050406
363400050415     c                   Except    tcodd4
363500050406
363600050406      ** Fattura inizio mese
363700050406    2c     fatfiv        wheneq    §qcfiv
363800050406     c                   if        *in45
363900050406     c  n11              add       2             nri
364000050406     c   11              add       3             nri
364001170202      * se la scritta dichiarazione intenti è oltre 113 caratteri
364002170202     c                   if        st_dicint2 <> *blank
364003170202     c                   add       1             nri
364004170202     c                   seton                                        46
364005170202     c                   endif
364006170202      * se la scritta dichiarazione intenti è oltre 226 caratteri
364007170202     c                   if        st_dicint3 <> *blank
364008170202     c                   add       1             nri
364009170202     c                   seton                                        47
364010170202     c                   endif
364100050406     c                   else
364200050406     c                   add       1             nri
364300050406     c                   endif
364400050406
364500050406    3c                   If        nri > wrighe
364600050406     c                   except    rcodi
364700050406     c                   except    hcodi2
364800050406     c                   if        *in45
364803170202     c  n11              z-add     2             nri
364804170202     c   11              z-add     3             nri
364805170202      * se la scritta dichiarazione intenti è oltre 113 caratteri
364806170202     c                   if        st_dicint2 <> *blank
364807170202     c                   add       1             nri
364808170202     c                   seton                                        46
364809170202     c                   endif
364810170202      * se la scritta dichiarazione intenti è oltre 226 caratteri
364811170202     c                   if        st_dicint3 <> *blank
364812170202     c                   add       1             nri
364813170202     c                   seton                                        47
364814170202     c                   endif
364815170202
365100050406     c                   else
365200050406     c                   z-add     1             nri
365300050406     c                   endif
365400050406     c
365500050406     c                   z-add     74            wrighe
365600050406    3c                   endif
365700050406
365800050406     c                   except    tcodi4
365900050406
366000050406      ** fattura fine mese
366100050406   X2c                   other
366200050406
366201170202     c                   if        *in45
366300050406     c  n11              add       2             nrf
366400050406     c   11              add       3             nrf
366401170202      * se la scritta dichiarazione intenti è oltre 113 caratteri
366402170202     c                   if        st_dicint2 <> *blank
366403170202     c                   add       1             nrf
366404170202     c                   seton                                        46
366405170202     c                   endif
366406170202      * se la scritta dichiarazione intenti è oltre 226 caratteri
366407170202     c                   if        st_dicint3 <> *blank
366408170202     c                   add       1             nrf
366409170202     c                   seton                                        47
366410170202     c                   endif
366411170202     c                   else
366412170202     c                   add       1             nrf
366413170202     c                   endif
366500050406
366600050406    3c                   If        nrf > wrighe
366700050406     c                   except    rcodf
366800050406     c                   except    hcodf2
366900050406     c                   if        *in45
367000050406     c  n11              z-add     2             nrf
367100050406     c   11              z-add     3             nrf
367101170202      * se la scritta dichiarazione intenti è oltre 113 caratteri
367102170202     c                   if        st_dicint2 <> *blank
367103170202     c                   add       1             nrf
367104170202     c                   seton                                        46
367105170202     c                   endif
367106170202      * se la scritta dichiarazione intenti è oltre 226 caratteri
367107170202     c                   if        st_dicint3 <> *blank
367108170202     c                   add       1             nrf
367109170202     c                   seton                                        47
367110170202     c                   endif
367200050406     c                   else
367300050406     c                   z-add     1             nrf
367400050406     c                   endif
367500050406
367600050406     c                   z-add     74            wrighe
367700050406    3c                   endif
367800050406
367900050406     c                   except    tcodf4
368000050406    2c                   endsl
368100050406    1c                   endif
368200050406
368300160623      ** STAMPA dicitura del peso vdl non più dal 23/06/16
368400050406
368500160623     c***                if        wcalcevdl='S'
368600160623     c***                eval      stavdl=cpesovdl
368700050406
368800160623     c***                select
368900050406      ** autofattura
369000160623    2c***  fattft        wheneq    1
369100160623     c***                add       1             nrd
369200160623    3c***                If        nrd > wrighe
369300160623     c***                except    rcodd
369400160623     c***                except    hcodd2
369500160623     c***                z-add     1             nrd
369600160623     c***                z-add     74            wrighe
369700160623    3c****               endif
369800050406
369900160623     c****               except    tcodd5
370000050406
370100050406      ** fattura inizio mese
370200160623    2c***  fatfiv        wheneq    §qcfiv
370300160623     c***                add       1             nri
370400160623    3c***                if        nri > wrighe
370500160623     c****               except    rcodi
370600160623     c****               except    hcodi2
370700160623     c****               z-add     1             nri
370800160623     c****               z-add     74            wrighe
370900160623    3c****               endif
371000050406
371100160623     c****               except    tcodi5
371200050406      ** fattura fine mese
371300160623   X2c***                other
371400160623     c****               add       1             nrf
371500160623    3c***                if        nrf > wrighe
371600160623     c***                except    rcodf
371700160623     c***                except    hcodf2
371800160623     c***                z-add     1             nrf
371900160623     c***                z-add     74            wrighe
372000160623    3c***                endif
372100050406
372200160623     c***                except    tcodf5
372300050406
372400160623    2c***                endsl
372500050406
372600160623     c***                endif
372700050406      ** verifco se la fattura ha più di tre righe di totale
372800050406     c                   setoff                                       34
372900050406     c     imd(4)        comp      0                                  34
373000050406     c   34              exsr      errcon
373100050406
373200050406      ** salto fine pagina
373300050406      * autofattura
373400050406     c                   If        Fattft = 1
373500050406     c                   z-add     0             trigtd
373600050406     c                   except    tcodd
373700050406     c                   else
373800050406      ** inizio mese
373900050406     c                   If        fatfiv = §qcfiv
374000050406     c                   z-add     0             trigti
374100050406     c                   except    tcodi
374200050406      ** fine mese
374300050406     c                   else
374400050406     c                   z-add     0             trigtf
374500050406     c                   except    tcodf
374600050406     c                   endif
374700050406
374800050406     c                   endif
374900050406      **
375000050406      ** Stampo totali per aliquota IVA
375100050406    1c                   do        5             a
375200050406     c                   setoff                                       89
375300050406     c                   z-add     0             varied
375400050406     c                   z-add     0             periva
375500050406    2c                   if        imd(a) > 0
375600050406    3c                   if        ivd(a) > 0
375700050406     c                   seton                                        89
375800050406     c                   move      ai(a)         wiva52
375900050406     c                   z-add     wiva52        periva
376000050406    3c                   endif
376100050406      * Varie
376200050406     c                   add(h)    v1d(a)        varied
376300050406      * Porto
376400050406     c                   z-add(h)  pod(a)        prttpid
376500060908      * Faccio in modo che le somme del traposto e delle varie dia lo stesso importo dell'imponibile
376600060908     c                   If        varied > 0 and prttpid > 0
376700060908     c                   eval      varied = imd(a) - prttpid
376800060908     c                   else
376900060908     c                   if        varied > 0
377000060908     c                   z-add     imd(a)        varied
377100060908     c                   endif
377200060908     c                   if        prttpid> 0
377300060908     c                   z-add     imd(a)        prttpid
377400060908     c                   endif
377500060908     c                   endif
377600050406      * Autofattura
377700050406    3c                   if        fattft = 1
377800050406     c                   add       1             trigtd
377900050406    4c                   if        trigtd > 3
378000050406     c                   except    rcodd2
378100050406     c                   except    hcodd2
378200050406     c                   except    tcodd
378300050406     c                   z-add     0             trigtd
378400050406    4c                   endif
378500050406     c                   except    tcodd2
378600050406      * Inizio mese
378700050406   x3c                   else
378800050406    4c                   if        fatfiv = §qcfiv
378900050406     c                   add       1             trigti
379000050406    5c                   if        trigti > 3
379100050406     c                   except    rcodi2
379200050406     c                   except    hcodi2
379300050406     c                   except    tcodi
379400050406     c                   z-add     0             trigti
379500050406    5c                   endif
379600050406     c                   except    tcodi2
379700050406      * Fine mese
379800050406   x4c                   else
379900050406     c                   add       1             trigtf
380000050406    5c                   if        trigtf > 3
380100050406     c                   except    rcodf2
380200050406     c                   except    hcodf2
380300050406     c                   except    tcodf
380400050406     c                   z-add     0             trigtf
380500050406    5c                   endif
380600050406     c                   except    tcodf2
380700050406
380800050406    4c                   endif
380900050406    3c                   endif
381000050406    2c                   endif
381100050406    1c                   enddo
381200050406
381300050406      ** Stampo totale generale fattura
381400000126     c                   setoff                                       60
381500050406     c                   if        fattft = 1
381600050406      * controllo se ho scritto 1 sola riga di aliquote iva
381700050406     c                   eval      *in60 =  (trigtd =  1)
381800050406     c                   except    tcodd3
381900050406     c                   else
382000050406     c                   if        fatfiv = §qcfiv
382100050406     c                   eval      *in60 =  (trigti =  1)
382200050406     c                   except    tcodi3
382300050406     c                   else
382400050406     c                   eval      *in60 =  (trigtf =  1)
382500050406     c                   except    tcodf3
382600050406     c                   endif
382700050406     c                   endif
382800050406
382900050406     c                   z-add     0             wstcod
383000050406
383100050406     c                   endsr
383200020320     C/SPACE
383300050407      ******************************************************             1
383400050407      ** Stampo legenda mettendo le decodifiche
383500050407      ******************************************************
383600050407     c     staleg        begsr
383700050407
383800050407    1c                   if        les(1) <> *blanks
383900050407
384000050407      ** ricerca delle descrizioni
384100050407    3c                   do        20            a
384200050407    1c                   if        les(a) <> *blanks
384300050407     c                   z-add     1             a2
384400050407     c                   movel     'VARIE   '    kcc
384500050407     c                   move      les(a)        kcc
384600050407     c                   if        varfed=' '
384700050407     c     kcc           lookup    cc(a2)                                 05
384800050407     c   05a2            occur     dscc
384900050407     c   05              movel     §ccdes        led(a)
385000050407     c                   else
385100050407     c     kcc           lookup    cb(a2)                                 05
385200050407     c   05a2            occur     dscb
385300050407     c   05              movel     §cbdsf        led(a)
385400050407     c                   endif
385500050407     c   05              movel     '='           lin(a)
385600050407    4c                   endif
385700050407    3c                   enddo
385800050407      **
385900050407      * imposto la dicitura iniziale
386000050407     c                   if        varfed=' '
386100050407     c                   movel     clegenda      decleg
386200050407     c                   else
386300050407     c                   movel     clegendafed   decleg           20
386400050407     c                   endif
386500050407      *
386600050407     c                   z-add     1             a
386700050407     c                   dow       A <= 20 and les(a) <> *blank
386800050407      *
386900050407     c                   select
387000050407      ** Autofattura
387100050407    4c     Fattft        Wheneq    1
387200050407    5c                   If        a = 1
387300050407     c                   add       2             nrd
387400050407     c                   else
387500050407     c                   add       1             nrd
387600050407    5c                   endif
387700050407      *
387800050407    5c                   if        nrd > wrighe
387900050407     c                   except    rcodd
388000050407     c                   except    hcodd2
388100050407     c                   z-add     2             nrd
388200050407     c                   z-add     74            wrighe
388300050407    5c                   endif
388400050407      *
388500050407    5c                   If        a = 1
388600050407     c                   except    dcodd4
388700050407     c                   else
388800050407     c                   if        a = 9
388900050407     c                   except    dcodd5
389000050407     c                   else
389100050407     c                   except    dcodd6
389200050407     c                   endif
389300050407    5c                   endif
389400050407      ** Fattura inizio mese
389500050407    4c     Fatfiv        Wheneq    §qcfiv
389600050407    5c                   if        a = 1
389700050407     c                   add       2             nri
389800050407     c                   else
389900050407     c                   add       1             nri
390000050407    5c                   endif
390100050407      *
390200050407    5c                   if        nri > wrighe
390300050407     c                   except    rcodi
390400050407     c                   except    hcodi2
390500050407     c                   z-add     2             nri
390600050407     c                   z-add     74            wrighe
390700050407    5c                   endif
390800050407      *
390900050407    5c                   If        a = 1
391000050407     c                   except    dcodi4
391100050407     c                   else
391200050407     c                   if        a = 9
391300050407     c                   except    dcodi5
391400050407     c                   else
391500050407     c                   except    dcodi6
391600050407     c                   endif
391700050407    5c                   endif
391800050407      ** Fattura fine mese
391900020320   X4C                   OTHER
392000050407    5c                   if        a = 1
392100050407     c                   add       2             nrf
392200050407     c                   else
392300050407     c                   add       1             nrf
392400050407    5c                   endif
392500050407      *
392600050407    5c                   if        nrf > wrighe
392700050407     c                   except    rcodf
392800050407     c                   except    hcodf2
392900050407     c                   z-add     2             nrf
393000050407     c                   z-add     74            wrighe
393100050407    5c                   endif
393200050407      *
393300050407    5c                   If        a = 1
393400050407     c                   except    dcodf4
393500050407     c                   else
393600050407     c                   if        a = 9
393700050407     c                   except    dcodf5
393800050407     c                   else
393900050407     c                   except    dcodf6
394000050407     c                   endif
394100050407    5c                   endif
394200050407      *
394300050407    4c                   endsl
394400050407      *
394500050407     c                   add       8             a
394600050407    3c                   enddo
394700050407      *
394800050407    1c                   endif
394900050407      *
395000050407     c                   endsr
395100050406      ******************************************************
395200050406      ** Scrittura    NDBH*    Movimenti contabili
395300050406      ******************************************************
395400050406     c     crepn         begsr
395500050406     c                   z-add     0             riga
395600050406      * gestione del controllo e conversione nella moneta di conto
395700050406     c                   exsr      preppn
395800050406      *
395900050406     c                   z-add     0             rigai
396000050406     c                   setoff                                       39
396100050406     c                   clear                   ndbhm000
396200050406      ** TIPO RECORD *A*
396300050406      * ctb generale ndbhm
396400050406     c                   movel     §qccaf        bhmcaustes
396500121003      * tolgo la registrazione con causale per gli 8888 in quanto non li fatturiamo
396600121003     c**n10              movel     §qcca8        bhmcaustes
396700050406     c                   movel     knraz         bhmrifint
396800121003     c                   movel     knraz         bhmknraz
396900050406     c                   z-add     fatnft        bhmsubnum
397000050406     C                   movel     fatfiv        bhmsubnum
397100050406     c                   move      wsocieta      bhmsocieta
397200050406     c                   move      'CG'          bhmctb
397300050406     c                   move      'CG'          bhmclasse
397400050406     c                   movel     fatksc        uni3              3
397500050406     c                   movel(p)  uni3          bhmunita                       uff.telecomunicaz
397600050406     c                   move      'D'           bhmtpregz
397700050406     c                   move      $kcc          bhmkcc
397800050406     c                   move      *zeros        bhmksc
397900050406     c                   move      fatksc        bhmksc
398000050406     c                   move      fatdft        bhmdtreg
398100050406     c                   z-add     fatnft        bhmnrreg
398200050406     c                   move      fatdft        bhmdtdoc
398300050406     C                   Z-ADD     fatNFT        bhmnrdoc
398400050406     C                   move      fatDFT        bhmdtpar
398500050406     C                   Z-ADD     fatNFT        bhmnrpar
398600050406     c                   add       1             riga
398700050406     c                   z-add     riga          bhmsubrig
398800050406     C                   movel     '2'           bhmserreg
398900050406     C                   move      fatFIV        bhmserreg
399000050406      *
399100050406     c                   movel     fatra1        bhmdesbrev
399200050406     c                   if        xscdiv = viddiv
399300050406     c                   movel     xscdiv        bhmdivisa
399400050406     c                   z-add     fattotd       bhmimportd
399500050406     c                   z-add     fattotd       bhmimporto
399600050406     c                   z-add     1             bhmcambio
399700050406     c                   else
399800050406     c                   movel     viddiv        bhmdivisa
399900050406     c                   z-add     fattotd       bhmimportd
400000050406     c                   z-add     fattotcnt     bhmimporto
400100050406     c                   z-add     cambio0202    bhmcambio
400200050406     c                   endif
400300050406     c                   move      'D'           bhmsegno
400400050406     c                   movel     fatcdp        bhmcondpag
400500050406     c                   write     ndbhm000
400600050406      **
400700050406      ** TIPO RECORD  *C*
400800050406      * rek  IVA ndbhi
400900050406     c                   do        5             a
401000050406     c                   clear                   ndbhi000
401100050406     c                   if        ai(a) <> *blank
401200050406     c                   move      ai(a)         wivads
401300050406     c                   if        fatsci <> *blank
401400050406     c                   move      fatsci        bhirifiva
401500050406     c                   else
401600050406     c                   move      fatali        bhiali
401700050406     c                   endif
401800050406     c                   movel     knraz         bhirifint
401900050406     c                   z-add     fatnft        bhisubnum
402000050406     c                   movel     fatFIV        bhisubnum
402100050406     c                   movel     wsocieta      bhisocieta
402200050406     c                   add       1             rigai
402300050406     c                   z-add     rigai         bhisubrig
402400050406     c                   move      'A'           bhisegno
402500050406     c                   z-add     imd(a)        bhiimpon
402600050406     c                   z-add     imd(a)        bhiimpond
402700050406     c                   z-add     ivd(a)        bhiimportd
402800050406     c                   z-add     ivd(a)        bhiimporto
402900050406     c                   if        xscdiv <> viddiv
403000050406     c                   z-add     imc(a)        bhiimpon
403100050406     c                   z-add     ivc(a)        bhiimporto
403200050406     c                   endif
403300050406     c                   move      fatFIV        bhilibro
403400050406     c                   move      '2'           bhitpreg
403500050406     c                   write     ndbhi000
403600050406     c                   endif
403700050406     c                   enddo
403800050406      **  BOLLI
403900050406     c                   if        fatbold > 0
404000050406     c                   z-add     1             a
404100050406     c                   move      'VARIE   '    kcc
404200050406     c                   move      '6'           kcc
404300050406     c     kcc           lookup    cc(a)                                  05
404400050406     c   05a             occur     dscc
404500050406     c   05§ccjei        ifne      *blank
404600050406     c                   move      §ccjei        bhirifiva
404700050406     c                   z-add     0             bhiali
404800050406     c                   movel     knraz         bhirifint
404900050406     c                   z-add     fatnft        bhisubnum
405000050406     C                   movel     fatFIV        bhisubnum
405100050406     c                   movel     wsocieta      bhisocieta
405200050406     c                   add       1             rigai
405300050406     c                   z-add     rigai         bhisubrig
405400050406     c                   move      'A'           bhisegno
405500050406     c                   z-add     fatBOLD       bhiimpon
405600050406     c                   z-add     fatBOLD       bhiimpond
405700050406     c                   if        xscdiv <> viddiv
405800050406     c                   z-add     fatbolcnt     bhiimpon
405900050406     c                   endif
406000050406     c                   z-add     0             bhiimporto
406100050406     c                   z-add     0             bhiimportd
406200050406     c                   move      fatFIV        bhilibro
406300050406     c                   move      '2'           bhitpreg
406400050406     c                   write     ndbhi000
406500050406     c                   endif
406600050406     c                   endif
406700050406      **
406800050406      ** TIPO RECORD  *D*
406900050406      * analitica ndbha
407000050406     c                   clear                   ndbha000
407100050406     c                   do        50            a
407200050406     c                   if        icpd(a) <> 0
407300050406      *
407400050406     c                   move      ccp(a)        dsconti
407500050406     c                   move      wkcc          bhmkcc
407600050406     c                   move      wksc          bhmksc
407700050406     c                   move      wvoc          bhmvoce
407800050406     c                   z-add     icpd(a)       bhmimporto
407900050406     c                   z-add     icpd(a)       bhmimportd
408000050406     c                   if        xscdiv <> viddiv
408100050406     c                   z-add     icpc(a)       bhmimporto
408200050406     c                   endif
408300050406     c                   z-add     fatNFT        bhmnrpar
408400050406     c                   move      fatDFT        bhmdtpar
408500050406     c                   add       1             riga
408600050406     c                   z-add     riga          bhmsubrig
408700050406     c                   move      'A'           bhmsegno
408800050406      * cond pagamento
408900050406     c                   movel     fatcdp        bhmcondpag
409000050406      * ricavo data di competenza = data spedizione
409100050406      * a cambio anno = data fatturazione
409200050406     c                   move      dtdsp         bhmdtcoan
409300050406     c                   movel     fatdft        wannoft           4 0
409400050406     c                   if        wannoft <> tasaas
409500050406     c                   move      fatdft        bhmdtcoan
409600050406     c                   endif
409700050406     c                   write     ndbhm000
409800050406      *
409900050406     c                   move      bhmdtcoan     bhadtcoan
410000050406     c                   move      bhmrifint     bharifint
410100050406     c                   move      bhmsubnum     bhasubnum
410200050406     c                   move      bhmsubrig     bhasubrig
410300050406     c                   move      bhmvoce       bhavoce
410400050406     c                   move      'D1'          bhadimen
410500050406     c                   movel     bhmunita      bhaentit1
410600050406     c                   move      wsocieta      bhasocieta
410700050406     c                   z-add     icpd(a)       bhaimporto
410800050406     c                   z-add     icpd(a)       bhaimportd
410900050406     c                   if        xscdiv <> viddiv
411000050406     c                   z-add     icpc(a)       bhaimporto
411100050406     c                   endif
411200050406     c                   move      'A'           bhasegno
411300050406     c                   add       1             bhasubsubr
411400050406     c                   write     ndbha000
411500050406     c                   endif
411600050406     c                   enddo
411700050406      ** TIPO RECORD  *D*
411800050406      * se moneta diversa tra la divisa di fatturazione e la moneta di conto verifico se esistono
411900050406      * delle differenze di cambio
412000050406     c                   if        xscdiv <> viddiv
412100050406     c                   xfoot     icpc          wtoticpc
412200050406      * sommo i  valori che mi devono dare il totale fattura
412300050406     c                   eval      wtotfat = wtoticpc + wtotivc
412400050406      * calcolo la differenza dei totali fattura tra il convertito ed il calcolato
412500050406     c                   eval      diffe = fattotcnt - wtotfat
412600050406      * SE ESISTE DIFFERENZA
412700050406      *
412800050406     c                   if        diffe <> 0
412900050406      *
413000050406      * controllo se diffe è positivo o negativo
413100050406     c                   if        diffe < 0                                     * ARR. PASSIVO
413200050406      * recupero conto + causale contabile
413300050406     c     2             OCCUR     DARR
413400050406     c                   move      'D'           bhmtpregz
413500050406     c                   mult      -1            diffe
413600050406     c                   else                                                    * ARR. ATTIVO
413700050406     c     1             OCCUR     DARR
413800050406     c                   move      'A'           bhmtpregz
413900050406     c                   endif
414000050406      * ctb generale ndbhm
414100050406     c                   movel     §arrcau       bhmcausrig
414200050406      * Danilo ha detto di lasciare nft0 (ma io faccio di testa  mia)
414300050406     c                   move      §arrkcc       bhmkcc
414400050406     c                   move      §arrksc       bhmksc
414500050406     c                   move      §arrvoc       bhmvoce
414600050406     c                   add       1             riga
414700050406     c                   z-add     riga          bhmsubrig
414800050406      *
414900050406     c                   movel     xscdiv        bhmdivisa
415000050406     c                   movel     xscdiv        bhmdivisam
415100050406     c                   z-add     1             bhmcambio
415200050406     c                   z-add     diffe         bhmimporto
415300050406     c                   z-add     diffe         bhmimportd
415400050406     c                   move      bhmtpregz     bhmsegno
415500050406     c                   write     ndbhm000
415600050406      *
415700050406     c                   clear                   ndbha000
415800050406     c                   move      bhmdtcoan     bhadtcoan
415900050406     c                   move      bhmrifint     bharifint
416000050406     c                   move      bhmsubnum     bhasubnum
416100050406     c                   move      bhmsubrig     bhasubrig
416200050406     c                   move      bhmvoce       bhavoce
416300050406     c                   move      'D1'          bhadimen
416400050406     c                   movel     §arrcdc       bhaentit1
416500050406     c                   move      wsocieta      bhasocieta
416600050406     c                   z-add     diffe         bhaimporto
416700050406     c                   move      bhmtpregz     bhasegno
416800050406     c                   add       1             bhasubsubr
416900050406     c                   write     ndbha000
417000050406      *
417100050406     c                   endif
417200050406      *
417300050406     c                   endif
417400050406      *----------------------------------*
417500050406      ** Creazione totali fatturazione   *
417600050406      *----------------------------------*
417700050406      * totale imponile
417800000107     c                   do        5             A
417900050406     c                   add       IMD(A)        impgen
418000050406      * totale iva
418100050406     c                   add       IVD(A)        ivagen
418200050406      * in moneta di conto
418300000124     c                   if        viddiv <> xscdiv
418400000124     c                   add       imc(a)        impgencnt
418500000125     c                   add       ivc(a)        ivagencnt
418600000124     c                   endif
418700050406      *
418800050406     c                   enddo
418900050406      * totale bolli
419000050406     c                   add       fatbold       bolgen
419100050406      * totale fatturato
419200050406     c                   add       fattotd       totgen
419300050406      * in moneta di conto
419400050406     c                   if        viddiv <> xscdiv
419500050406     c                   add       fatbolcnt     bolgencnt
419600050406     c                   add       fattotcnt     totgencnt
419700050406     c                   endif
419800050406      * numero fatture
419900050406     c                   add       1             numgen
420000050406      *----------------------------------*
420100050406      ** stampa etichette                *
420200050406      *----------------------------------*
420300050406      * se ha l'etichetta
420400051128     c                   if        wanspe = 'S'  and
420500101005      * tolgo il controllo delle 50 pagine per invio mail
420600101005     c****                         (wmail = ' ' or (wmail = 'S' and page >= 50))
420700101005     c                              wmail = ' '
420800050406      * scrivo record su ETLAV00F
420900050406     c                   clear                   etlav
421000050406      * codice cliente bollettazione se esiste uno di fatturazione prendo quello salvato
421100050406      * altrimenti prendo quello che ho in linea
421200050406     c                   IF        savtasksc > *blanks
421300050406     c                   move      savtasksc     ETLBOL
421400050406     c                   move      ktasksc       ETLFAT
421500050406     c                   else
421600050406     c                   move      ktasksc       ETLBOL
421700050406     c                   move      ktasksc       ETLFAT
421800050406     c                   endif
421900050406      * numero fattura
422000050406     c                   z-add     fatnft        ETLNDC
422100050406      * AS400 FATTURA
422200050406     c                   move      asfat         ETLASF
422300050406      *
422400060214      * scrivo record su ETLAV00F
422500050406     c                   write     ETLAV
422600050406      *
422700050406     c                   endif
422800051128
422900050406     c                   endsr
423000060214      ***********************************************************************
423100060214      ** PREPARAZIONE riepilogo tipo invio fattura
423200060214      ***********************************************************************
423300060214     c     Wrtift        begsr                                                                     i
423400060214      *
423500060214     c                   clear                   Tiift000
423600060214
423700060214     c                   z-add     fatnft        iftnft                         numero fattura
423800060214     c                   z-add     fatdft        iftdft                         data fattura
423900060214     c                   z-add     bolksc        iftcbo                         codice bollettazione
424000060214     c                   z-add     fatksc        iftcfa                         codice fatturazione
424100060214     c                   move      fatifm        ifttdf                         Inizio fine mese
424200060214     c                   move      fattft        ifttft                         tipo 1=Dist. 0=fatt.
424300060217     c                   move      fatfft        iftfft                         Freq.fattura (bol)
424400060217     c                   z-add     dtdsp         iftdsp                         data spedizione
424500060214     c                   z-add     page          iftpag                         pagine
424600060214     c                   z-add     fatdir        iftsif                         spese invio fattura
424700101005     c                   move      indmail       iftind                         indirizzo mail
424800060307      * verifico se c'è etichetta da stampare oppure no
424900060307     c                   if        wanspe = 'S'
425000060307     c                   eval      ifteti = 'SI'
425100060307     c                   else
425200060307     c                   eval      ifteti = 'NO'
425300060307     c                   endif
425400060307      * tipo invio
425500060307     c                   SELECT
425600060307      * se è una distinta va su carta
425700060307     c                   when      fattft = 1
425800060307     c                   eval      ifttif = 'C'                                 Tipo invio Carta
425900060307      * se c'è §*§ carta
426000060307     c                   When      wlbeti <> *blank
426100060307     c                   eval      ifttif = 'C'                                 Tipo invio Carta
426200101005      * se c'è invio mail
426300101005      * non controllo + se ho + di 50 pagine invio sempre via e-mail
426400101005     c**************     When      wmail = 'S' and page < 50
426500101005     c                   When      wmail = 'S'
426600060307     c                   eval      ifttif = 'M'                                 Tipo invio Mail
426700060307      * se c'è invio mail ed ho più  di 49 pagine
426800101005     c**************     When      wmail = 'S' and page > 49
426900101005     c**************     eval      ifttif = 'C'                                 Tipo invio Carta
427000060307
427100060307      * altrimenti
427200060307     c                   OTHER
427300060307
427400060307      * postel se ho meno di 50 pagine
427500060307     c                   if        page <  50
427600060307     c                   eval      ifttif = 'P'                                 Tipo invio Postel
427700060307     c                   else
427800060307      * carta se ho più di 50 pagine
427900060307     c                   eval      ifttif = 'C'                                 Tipo invio Carta
428000060307     c                   endif
428100060307
428200060307     c                   ENDSL
428201170127      * valorizzo i campi relativi al totale fattura
428202170127     c                   eval      iftfiv = tasfiv
428203170127     c                   eval      ifttot = fattotd
428204170127     c                   eval      iftbol = fatbold
428205170127     c                   eval      iftimp1 = imd(1)
428206170127     c                   move      ai(1)         wivads
428300170127     c                   move      fatsci        iftcei1
428301170127     c                   move      fatali        iftali1
428302170202     c                   eval      iftiva1 = ivd(1)
428303170127     c                   eval      iftimp2 = imd(2)
428304170127     c                   move      ai(2)         wivads
428305170127     c                   move      fatsci        iftcei2
428306170127     c                   move      fatali        iftali2
428307170202     c                   eval      iftiva2 = ivd(2)
428308170127     c                   eval      iftimp3 = imd(3)
428309170127     c                   move      ai(3)         wivads
428310170127     c                   move      fatsci        iftcei3
428311170127     c                   move      fatali        iftali3
428312170202     c                   eval      iftiva3 = ivd(3)
428313170127     c                   move      dicesito      iftdic
428314170127     c                   move      fativa        iftpiva
428400060217     c                   write     Tiift000
428500060214
428600060214     c                   endsr
428700050406      ***********************************************************************
428800050406      ** PREPARAZIONE IMPORTI PER REGISTRAZIONI PRIMA NOTA IN MONETA DI CONTO
428900050406      ***********************************************************************
429000060217     c     preppn        begsr
429100050406      *
429200050406     c                   clear                   fattotcnt
429300050406     c                   clear                   imc
429400050406     c                   clear                   ivc
429500050406     c                   clear                   icpc
429600050406     c                   clear                   fatbolcnt
429700050406     c                   clear                   wtotimc
429800050406      *
429900050406      * se moneta di conto diversa dalla moneta di fatturazione  conevrsione degli
430000050406      * importi
430100050406     c                   if        xscdiv <> viddiv
430200050406      *
430300050406      * CONVERSIONE
430400050406      * TOTALE FATTUTA
430500050406      *******   POTREBBE NON SERVIRE ********************************************
430600050406     c                   eval      div1 = viddiv                                *
430700050406     c                   eval      div2 = xscdiv                                *
430800050406     c                   reset                   PJX00202DS                     *
430900050406     c                   z-add     fattotd       IMPORD0202                     *
431000050406     c                   exsr      cnveur                                       *
431100050406     c                   z-add     imporm0202    fattotcnt                      *
431200050406      ***************************************************************************
431300050406      * BOLLI
431400050406     c                   if        fatbold > 0
431500050406     c                   eval      div1 = viddiv
431600050406     c                   eval      div2 = xscdiv
431700050406     c                   reset                   PJX00202DS
431800050406     c                   Z-ADD     fatbold       IMPORD0202
431900050406     c                   exsr      cnveur
432000000107     c                   z-add     IMPORM0202    fatbolcnt
432100000105     c                   endif
432200050406      * IMPONIBILI IVA
432300050406      *
432400050406     c                   do        5             a
432500050406     c                   if        imd(a) > 0
432600050406     c                   eval      div1 = viddiv
432700050406     c                   eval      div2 = xscdiv
432800050406     c                   reset                   PJX00202DS
432900050406     c                   z-add     imd(a)        IMPORD0202
433000050406     c                   exsr      cnveur
433100050406     c                   z-add     IMPORM0202    imc(a)
433200050406     c                   endif
433300050406     c                   enddo
433400050406      * IVA
433500050406     c                   DO        5             A
433600050406     c                   IF        ivd(a) > 0
433700050406     c                   eval      div1 = viddiv
433800050406     c                   eval      div2 = xscdiv
433900050406     c                   reset                   PJX00202DS
434000050406     c                   z-add     ivd(a)        IMPORD0202
434100050406     c                   exsr      cnveur
434200050406     c                   z-add     IMPORM0202    ivc(a)
434300050406     c                   endif
434400050406     c                   enddo
434500050406      * IMPORTI CONTROPARTITE
434600050406     c                   DO        50            A
434700050406     c                   IF        icpd(a) > 0
434800050406     c                   eval      div1 = viddiv
434900050406     c                   eval      div2 = xscdiv
435000050406     c                   reset                   PJX00202DS
435100050406     c                   Z-ADD     icpd(a)       IMPORD0202
435200050406     c                   exsr      cnveur
435300050406     c                   z-add     IMPORM0202    icpc(a)
435400050406     c                   endif
435500050406     c                   enddo
435600050406      *
435700050406     c                   endif
435800050406      * calcolo il totale fattura da attribuire al cliente
435900050406      * sommo imponibli iva
436000050406     c                   xfoot     imc           wtotimc
436100050406      * sommo iva
436200050406     c                   xfoot     ivc           wtotivc
436300050406      * sommo i  valori che mi devono dare il totale fattura
436400050406     c                   eval      fattotcnt = wtotimc + wtotivc + fatbolcnt
436500050406      *
436600050406      * SE MONETA DI CONTO UGUALE ALLA MONETA DI FATTURAZIONE
436700050406      * VERIFICO SE CI SONO DIFFERENZE TRA LE IMPUTAZIONI DI CONTO E IL TOTALE FATTURA
436800050406      * DATA LA PRESENZA DI IMPORTI CON NUMERI DI DECIMALI DIVERSI
436900050406      *
437000050406     c                   if        xscdiv = viddiv
437100050406      * calcolo totale contropartite
437200050406     c                   xfoot     icpd          wtoticpd
437300050406      * calcolo totale iva
437400050406     c                   xfoot     ivd           wtotivd
437500050406      * calcolo il totale fattura
437600050406     c                   eval      wtotfatd = wtoticpd + wtotivd
437700050406      * calcolo la differenza tra il totale fattura calcolato e quello da imputare
437800050406      * al cliente ....
437900050406     c                   eval      diffe    = fattotd - wtotfatd
438000050406      *
438100050406      * se differenza diversa da zero aggiusto il primo importo che trovo nella contropartita
438200050406      *
438300050406     c                   if        diffe <> 0
438400050406      * cerco il primo importo maggiore di zero
438500050406     c                   DO        50            A
438600050406     C                   IF        icpd(a) > 0
438700050406     C                   add       diffe         icpd(a)
438800050406     c                   z-add     51            a
438900050406     c                   endif
439000050406     c                   enddo
439100050406      *
439200050406     c                   clear                   diffe
439300050406      *
439400050406     c                   endif
439500050406     c                   endif
439600050406
439700050406      *
439800050406     c                   endsr
439900050406      *
440000050330      ***********************************************************
440100050330      ** Memorizzo su WFASC00F le bolle con Ass. e Ass. per conto
440200050330      ***********************************************************
440300050620     c***!!!***    Memass        begsr
440400050330
440500050330      * valorizzo i campi del file di lavoro
440600050330
440700050620     c***!!!***          clear                   wfasc000
440800050330
440900050620     c***!!!***          z-add     taslnp        asclnp
441000050620     c***!!!***          z-add     tasnrs        ascnrs
441100050620     c***!!!***          z-add     tasnsp        ascnsp
441200050620     c***!!!***          movel     tastbl        asctbl
441300050620     c***!!!***          z-add     tasksc        ascksc
441400050620     c***!!!***          z-add     tasmgs        ascmgs
441500050620     c***!!!***          z-add     tasaas        ascaas
441600050620     c***!!!***          movel     tasdiv        ascvas
441700050620     c***!!!***          z-add     tasial        ascias
441800050620     c***!!!***          z-add     stapkf        ascpkg
441900050330      * 87 ON --> Cliente codificato ma vario per l'assicurazione
442000050330      *   senza mandato o con mandato inferiore all'importo indicato
442100050330      *   in bolla
442200050620     c***!!!***          If        *in87
442300050620     c***!!!***          move      8888          ascksc
442400050620     c***!!!***          movel     bolra1        ascrsd
442500050620     c***!!!***          movel     bolvia        ascind
442600050620     c***!!!***          movel     bolcap        asccap
442700050620     c***!!!***          movel     bolcit        ascloc
442800050620     c***!!!***          endif
442900050330
443000050620     c***!!!***          z-add     1             a
443100050620     c***!!!***    §$2vrr        lookup    sv(a)                                  05
443200050620     c***!!!***  05              z-add     va(a)         ascvrr
443300050330      * valorizzo la divisa della varia "R"
443400050620     c***!!!***  05              move      tasdiv        ascvvr
443500050330
443600050620     c***!!!***          write     wfasc000
443700050620     c***!!!***          setoff                                       87
443800050330
443900050620     c***!!!***          endsr
444000050406      *********************************************************
444100050406      ** STAMPA ELENCO FATTURE - DISTINTE SPEDIZIONE
444200050406      *********************************************************
444300050406     c     wtrdis        begsr
444400050406
444500050406      ** apertura file di stampa
444600050406     c                   If        not *in50
444700050406     c                   open      disspe
444800050406     c                   z-add     66            condis
444900050406     c                   seton                                        50
445000050406     c                   endif
445100050406      **
445200050406     c                   xfoot     imd           timd
445300050406      ** sommo gli importi totale distinte di spedizione
445400050406     c                   add       timd          tdspd
445500050406      **
445600050406     c                   add       2             condis
445700050406     c                   if        condis > 60
445800050406     c                   except    hdgdis
445900050406     c                   z-add     2             condis
446000050406     c                   endif
446100050406     c                   except    detdis
446200100708     C
446300100708     C* MEMORIZZO IL DATO ANCHE SU FILE
446400100708     C                   CLEAR                   wfdss000
446500100708     c                   eval      dsstft=vidtft
446600100708     c                   eval      dssdfi=dtdfi8
446700100708     c                   eval      dsskscb=bolksc
446800100708     c                   eval      dsskscf=fatksc
446900100708     c                   eval      dssrag =fatra1
447000100708     c                   eval      dsstimd=timd
447100100708     c                   eval      dssdiv =viddiv
447200100708     c                   eval      dssnspe=numspe
447300100708     c                   write     wfdss000
447400050406      *
447500050406     c                   clear                   numspe
447600050406      *
447700050406     c                   endsr
447800050407      **---------------------------------------------------
447900050407      ** AZZERAMENTO CAMPI COMODO
448000050407      **---------------------------------------------------
448100080509     c     blank         begsr
448200050407
448300050407     c                   clear                   fatbold
448400050407     c                   clear                   totbold
448500050407     c                   clear                   impbold
448600050407     c                   clear                   prtpord
448700050407     c                   clear                   timd
448800050407     c                   clear                   prttpid
448900050407     c                   clear                   fattotcnt
449000050407     c                   clear                   fatbolcnt
449100050407     c                   clear                   dspult
449200050407     c                   clear                   dsppri
449300050407     c                   clear                   fatra1
449400050407     c                   clear                   fatra2
449500050407     c                   clear                   fatcit
449600050407     c                   clear                   fatvia
449700050407     c                   clear                   fatprv
449800050407     c                   clear                   fatbna
449900050407     c                   clear                   fatcap
450000050407     c                   clear                   fatsta
450400050407     c                   clear                   ai
450500050407     c                   clear                   imd
450600050407     c                   clear                   imdw
450700050407     c                   clear                   ivd
450800050407     c                   clear                   v1d
450900050407     c                   clear                   pod
451000050407     c                   clear                   imc
451100050407     c                   clear                   ivc
451200050407     c                   clear                   icpd
451300050407     c                   clear                   icpdw
451400050407     c                   clear                   icpc
451500050407     c                   clear                   ccp
451600050407     c                   clear                   fcp
451700050407     c                   clear                   S
451800050407     c                   clear                   id
451900050407     c                   clear                   s2
452000050407     c                   clear                   i2d
452100050407     c                   clear                   dirfatt
452200060214     c                   clear                   fatdir
452300050407     c                   clear                   CAL_vard
452400050407     c                   clear                   STP_vard
452500050407     c                   clear                   variadd
452600050407     c                   clear                   fatsbl
452700050407     c                   clear                   fattotd
452800050407     c                   clear                   varied
452900050407     c                   clear                   periva
453000050407
453100050408     c                   clear                   pdfi
453200050408     c                   clear                   pvia
453300050408     c                   clear                   pcap
453400050408     c                   clear                   ploc
453500050408     c                   clear                   ptel
453600050408     c                   clear                   pprv
453700050408     c                   clear                   pfax
453800050407     c                   clear                   cit3
453900050407
454000050408     c                   setoff                                       15
454100050408     c                   clear                   wstcod
454200050421     c                   clear                   wstbsc
454300050408
454400050408     c                   clear                   bolra1                         RAG.SOCIALE
454500050408     c                   clear                   bolVIA                         VIA
454600050408     c                   clear                   bolCIT                         CITTA
454700050408     c                   clear                   bolCAP                         CAP
454800050408     c                   clear                   bolPRV                         PROVINCIA
454900050408     c                   clear                   bolSTA                         NAZIONE
455000050408     c                   clear                   bolCAE
455100050408     c                   clear                   stpRMO
455200050423     c                   clear                   bscra1                         RAG.SOCIALE
455300050423     c                   clear                   bscVIA                         VIA
455400050423     c                   clear                   bscCIT                         CITTA
455500050423     c                   clear                   bscCAP                         CAP
455600050423     c                   clear                   bscPRV                         PROVINCIA
455700050423     c                   clear                   bscSTA                         NAZIONE
455800050423     c                   clear                   bscCAE
455900050423
456000050423     c                   clear                   tasbscpre
456100050423     c                   clear                   tasrscpre
456200050423     c                   clear                   tasfrgpre
456300120516     c                   clear                   tasfslpre
456400050408
456500050408     c                   clear                   wcalcevdl
456600050620
456700050408      *
456800050408     c                   endsr
456900050407      ********************************************************
457000050407      **  Ricerca in Azorg filiale incasso
457100050407      ********************************************************
457200050407     c     organi        begsr
457300050407
457400050407     c     FILPAG        chain     azorg01l
457500050407     c                   If        %found(azorg01l)
457600050408     c                   movel     orgdes        pdfi                           DESCR.FILIALE
457700050408     c                   movel     orgind        pvia                           VIA
457800050408     c                   movel     orgcpf        pcap                           CAP
457900050408     c                   movel     orgloc        ploc                           LOCALITA
458000050408     c                   movel     orgtel        ptel                           TELEFONO
458100050408     c                   movel     orgpro        pprv                           PROV.
458200050408     c                   movel     orgfax        pfax                           FAX
458300050415     c                   endif
458400050415
458500050407     c                   ENDSR
458600050405      ******************************************************
458700050405      **    Eseguo calcolo  IVA
458800050405      ******************************************************
458900050405     c     caliva        begsr
459000050405
459100050405     c                   z-add     0             impbold
459200111014     c                   z-add     0             impIVAd
459300050405     c                   z-add     0             aliiva
459400050405
459500050405      ** massimo 5 aliquote IVA
459600050405
459700050405     c                   do        5             a
459800050405     c                   movel     ai(a)         wivads
459900050405     c                   if        fatsci <> *blank
460000050405     c                   move      fatsci        rivrifiva
460100050405     c                   z-add     0             rivaliq
460200050405     c                   movel     §qcriv        wtpreg                         REG.IVA VEND.
460300050405      ** descrizione esenzione IVA
460400050405     c     Kriv          chain     anriv01l                           21
460500050405     c                   If        not  %found(anriv01l)
460600050405     c                   movel(p)  '********'    des(a)
460700050405     c                   else
460800050405     c                   movel(p)  rivdesbrev    des(a)
460900050405     c                   endif
461000050405      * se intestatario della fattura Estero (01 ON) non calcolo imponibile imposta bollo
461100120626      * Schettini ha fatto richiesta di apllicare l'imposta di bollo a tutti i clienti compresi
461200120626      * i clienti esteri 25/06/2012
461300120626     c***                If        not *IN01
461400050622      * verifico se codice di esenzione soggetto all'imposta di bollo oppure no
461500050622     c                   clear                   dsbs02
461600050622     c                   movel     'L'           T02tla
461700050622     c                   movel     'C'           T02mod
461800050622     c                   movel     knsif         t02sif
461900050622     c                   movel     'YBO'         t02cod
462000050622     c                   movel     fatsci        t02ke1
462100050622     c                   call      'TIBS02R'
462200050622     c                   parm                    KPJBA
462300050622     c                   parm                    DSBS02
462400050622      * se esenzione iva non presente in tabella calcolo imponibile per applicazione del bollo
462500050622     c                   if        t02err <> *blanks
462600111014     c                   add(h)    imdw(a)       impbold
462700050622     c                   Endif
462800050622
462900120626     c***                Endif
463000050405      ** se esente IVA   memorizzo imponibile per calcolo bollo
463100050405     c                   else
463200050405     c                   z-add     0             como15
463300050405     c                   move      fatali        aliiva
463400111014     c                   z-add(h)  imdw(a)       impIvad
463500050405      ** calcolo IVA
463600111014     c     ImpIvad       mult      aliiva        como15
463700050405     c                   If        como15 > 0
463800050405     c     como15        div(h)    100           ivd(a)
463900050405     c                   endif
464000050405     c                   endif
464100050405
464200050405     c                   enddo
464300050405
464400050405     c                   z-add     0             aliiva
464500050405
464600050405     c                   endsr
464700050405
464800050413      ********************************************************
464900050413      * ESEGUE IMPOSTAZIONE, RICERCA, MEMORIZZAZIONE
465000050413      * CONTI E IMPORTI DI CONTROPARTITA CONTABILITA' E IVA
465100050413      *  CONRIC:
465200050413      ** RICERCA IL CONTO NELLA DS -DSFC
465300050413      ** RELATIVA AL TIPO VOCE /TIPO SERVIZIO/TIPO BOLLA/IT-EST
465400050413      *  CONMEM:
465500050413      ** MEMORIZZO IL CONTO E IL RELATIVO IMPORTO
465600050413      ********************************************************
465700050413     c     CON001        BEGSR
465800050413     c                   SETOFF                                       08
465900050413      * rimetto l'"esenzione di testata" se cli codificato
466000050413     c                   move      FATSCIt       fatsci
466100050413      * SOLO PER CODIFICATO ESENTE PRENDO FATSCI
466200050413     c                   If        fatsci > *blank
466300050413     c                   move      FATSCI        eseiva
466400050413     c                   z-add     0             aliiva
466500050413     c                   ELSE
466600050413      *
466700050413      * ALTRIMENTI VEDO QUALE IVA APPLICARE
466800110909      * in base alla data fattura cerco l'aliquota in vigore
466900110909     c                   If        fatdft > §qtdsa
467000050413     c                   z-add     §Qtali        aliIVA            5 2
467100110909     c                   else
467200110909     c                   z-add     §Qtalo        aliIVA
467300110909     c                   endif
467400050413     c                   MOVEL     *blank        eseIVA            3
467500050413     c                   ENDIF
468500050413      *
468501160928      * carico le varie
468502160928     c                   exsr      carta7
468503160928      * verifico se devo calcolare la varia "D"
468504160928      * se non era tassata fino l'imponibile e non ha la varia 'D' e la devo ancora calcolare
468505160928    2c                   If        cal_vard = ' '  and tasfei = ' '
468506160928      * verifico se già calcolata in quella bolla
468507160928     c                   z-add     1             xx
468508160928     c     'D'           lookup    sv(xx)                                 13
468509160928    3c                   If        %found
468510160928     c                   eval      cal_vard = 'N'
468511160928     c                   eval      stp_vard = 'N'
468512160928
468513160928      * mi salvo il diritto di fatturazione da mettere in TIIFT
468514160928     c                   z-add     va(xx)        fatdir
468515160928
468516160928     c                   else
468517160928      * se non trovata la devo calcolare
468518160928     c                   eval      tassva  = 'D'
468519160928     c                   z-add     dirfatt       tasivdf
468520160928     c                   eval      tassvdf = 'D'
468521160928      *
468522160928     c                   call      'TNSF20R'
468523160928     C                   parm                    Kpjba
468524160928     c                   parm                    dtas
468525160928     c                   parm                    dtasv
468526160928      *
468527160928     c                   if        taserr = ' '
468528160928     c                   eval      cal_vard = 'N'
468529160928
468530160928      * mi salvo il diritto di fatturazione da mettere in TIIFT
468531160928     c                   z-add     dirfatt       fatdir
468532160928
468533160928     c                   endif
468534160928      *
468535160928    3c                   endif
468536160928
468537160928    2c                   endif
468600050413      **
468700050413      * IMPOSTO CONTO IVA DELLA SPEDIZIONE
468800050413     c     TASFEI        IFNE      *BLANKS
468900050413     c                   Z-ADD     1             A
469000050413     c     TASFEI        LOOKUP    CEI(A)                                 05
469100050413     c  N05              DO
469200050413     c                   MOVEL     E(5)          TASMSG
469300050413     c                   SETON                                        08
469400050413     c                   EXSR      errcon
469500050413     c                   SETOFF                                       08
469600050413     c                   END
469700050413     c   05              DO
469800050413     c     A             OCCUR     DSEI
469900050413     c                   z-add     0             aliIVA
470000050413     c                   MOVEL     §EIjei        eseIVA
470100050413     c                   MOVEL     §EIdes        DESESIVA
470200050413     c                   END
470300050413     c                   END
470400050413      ** CONTO IVA DELLA SPEDIZIONE
470500050413      *---------------------------------------------------------
470600050413     c                   MOVE      *BLANKS       KCC                            PORTO
470700050413     c                   MOVE      '1'           KCC
470800050413     c                   Z-ADD     TASPOR        WIMPOR
470900050413     c                   EXSR      CONRIC
471000050413     c                   EXSR      CONMEM
471100050413      *
471200981022     c                   exsr      bucoiva
471300050413      ** B INDICE ALIQUOTA IVA
471400050413     c                   ADD       TASPOR        POD(B)
471500050413     c                   ADD       TASPOR        IMDW(B)
471600050413      *---------------------------------------------------------
471700050413     c                   DO        20            K
471800050413     c     SV(K)         IFNE      *BLANKS
471900050413     c                   MOVEL     'VARIE   '    KCC
472000050413     c                   MOVE      SV(K)         KCC                            VARIA 1
472100050413     c                   Z-ADD     VA(K)         WIMPOR
472200050413     c                   EXSR      CONRIC
472300050413     c                   EXSR      CONMEM
472400050413      *  CTB
472500981022     c                   exsr      bucoiva
472600050413      ** B INDICE ALIQUOTA IVA
472700050415     c                   ADD       VA(K)         V1D(B)
472800050415     c                   ADD       VA(K)         IMDW(B)
472900050413     c                   END
473000050413     c                   END
473100050413      *
473200050413     c                   ENDSR
473300050413      *--------------------------------------------------
473400050413     c     bucoiva       begsr
473500050413      *--------------------------------------------------
473600050413     c     §ccjei        IFNE      *blank
473700050413     c                   MOVEL     §ccjei        fatsci
473800050413     c                   move      *zeros        fatali
473900050413     c                   ELSE
474000050413     c                   move      aliiva        fatali
474100050413     c                   move      eseiva        fatsci
474200050413     c                   END
474300050413      ** CONTO IVA
474400050413     c                   DO        5             B                 3 0
474500050413     c     AI(B)         COMP      WIVAds                                 05
474600050413     c   05              GOTO      EX1
474700050413     c     AI(B)         IFEQ      *blank
474800050413     c                   move      WIVAds        AI(B)
474900050413     c                   GOTO      EX1
475000050413     c                   END
475100050413     c                   END
475200050413     c     EX1           TAG
475300981021     c                   endsr
475400050405      *****************************************************************
475500050405      *    Ricerca conto relativo al tipo importo e spedizione        *
475600050405      *****************************************************************
475700050405     c     Conric        Begsr
475800050405
475900050405     c                   Setoff                                       08
476000050405     c                   z-add     1             a
476100050405     c     kcc           lookup    cc(a)                                  05
476200050405      * se non trovato in tabella CC Errore
476300050405     c                   If        not *in05
476400050405     c                   movel     e(4)          tasmsg
476500050405     c                   seton                                        08
476600050405     c                   exsr      errcon
476700050405     c                   setoff                                       08
476800050405     c                   z-add     1             a
476900050405     c                   endif
477000050405      **
477100050405     c     A             occur     dscc
477200050405     c                   move      §ccflp        wfcp                           FLAG COMB. FIL.
477300050406      **  verifico linea di partenza
477400050406     c                   If        taslnp <> savlnp
477500050406     c     taslnp        chain     azorg01l
477600050406     c                   if        %found(azorg01l)
477700050406     c                   movel     orgpro        cit2
477800050406     c                   movel     orgfl1        lnpfl1
477900050406     c                   movel     orgde3        og143
478000050406     c                   eval      lnpdpd = §ogdpd
478100050406     c                   else
478200050406     c                   clear                   cit2
478300050406     c                   clear                   lnpfl1
478400050406     c                   clear                   lnpdpd
478500050406     c                   endif
478600050406
478700050406     c                   movel     taslnp        savlnp
478800050406     c                   endif
478900050406      ** verifico linea di arrivo
479000050406     c                   If        taslna <> savlna
479100050406     c     taslna        chain     azorg01l
479200050406     c                   if        %found(azorg01l)
479300050406     c                   movel     orgfl1        lnafl1
479400050406     c                   else
479500050406     c                   clear                   lnafl1
479600050406     c                   endif
479700050406
479800050406     c                   movel     taslna        savlna
479900050406     c                   endif
480000050406      **
480100050406     c                   if        lnpfl1 = 'E' or lnafl1 = 'E'
480200050406      * estero
480300050406     c                   move      §ccjce        WKCC
480400050406     c                   move      §ccjse        WKSC
480500050406     c                   move      §ccjve        Wvoc
480600050406
480700050406     c                   else
480800050406      * italia
480900050406     c                   move      §ccjci        WKCC
481000050406     c                   move      §ccjsi        WKSC
481100050406     c                   move      §ccjvi        Wvoc
481200050406     c                   endif
481300050406      *
481400050406     c                   endsr
481500050406
481600050406      *****************************************************************
481700050406      *    memorizza importi nella schiera dei conti
481800050406      *****************************************************************
481900050406     c     conmem        begsr
482000050406
482100050406     c                   movel     dsconti       wconto
482200050406
482300050406     c                   z-add     1             a
482400050406     c     wconto        lookup    ccp(a)                                 05
482500050406     c  n05              z-add     1             a
482600050406     c  n05*blank        lookup    ccp(A)                                 05
482700050406     c                   movel     wconto        ccp(a)
482800050406     c                   add       wimpor        icpdw(a)
482900050406     c                   z-add     wfcp          fcp(a)
483000050406
483100050406     c                   endsr
483200050413      ********************************************************
483300050413      *   CONTROLLO protocollo dichiarazione d'intento
483400050413      ********************************************************
483500050413     c     protocollo    begsr
483600050413      *
483700000217     c                   clear                   wrkdff
483800981021      * testa data fine se piena e valida
483900981026     c                   if        *in20
484000981026     c     *eur          test(d)                 viddff                 86
484100981026     c                   if        *in86
484200981026     c     *dmy          test(d)                 viddff                 86
484300981026     c     *dmy          move      viddff        dataeur
484400981026     c                   else
484500981026     c                   move      viddff        dataeur
484600981026     c                   endif
484700981026     c                   else
484800981026     c     *eur          test(d)                 viddft                 86
484900981021     c                   if        *in86
485000981026     c     *dmy          test(d)                 viddft                 86
485100981026     c     *dmy          move      viddft        dataeur
485200981021     c                   else
485300981026     c                   move      viddft        dataeur
485400981021     c                   endif
485500981026     c                   endif
485501170127
485502170127     c                   move      dataeur       wrkdff
485503170126      * aggancio il file di lavoro delle dichiarazioni per vedere se ho già dei dati relativi al
485504170126      * codice intestazione fattura
485505170127     c     fatksc        chain     wfdicf1l
485506170126     c                   if        not %found(wfdicf1l)
485507170126      * chiamo i programmi di Cussini
485508170127     c                   exsr      Sr_intento
485509170127     c                   feod      wfdicf1l
485510170127      * riaggancio il record per aggiornarlo ai totali
485511170127     c     fatksc        chain     wfdicf1l
485512170127      *
485513170202     c                   endif
485514170203     c                   if        dicesito = 'S' and wf_Wtasfei = ' '
485515170207     c                   if        (dicprgfat+wf_wtasimv+dirfatt) > dicplafon
485517170207     c                              and  dicplafon > 0
485518170127     c                   eval      dicesito = 'N'
485519170207     c                   add       dirfatt       dicprgfat
485520170127     c                   add       wf_wtasimv    dicprgfat
485521170127     c                   update    wfdicf00
485522170127     c                   endif
485523170127     c                   endif
485524170127
487900170127      * se dichiarazione di intento attiva forzo fattura fine mese
487901170127     c                   if        DicEsito = 'S'
488000981021     C                   MOVEL     'F'           FATIFM
488100981021     c                   move      diccdiva      fatsci
488200990218     c                   move      diccdiva      fatscit
488300981021     c                   clear                   fatali
488301170202     c                   movel     dicfrase      st_dicint1
488302170203     c                   eval      st_dicint2 = %subst(dicfrase: 114: 113)
488303170203     c                   eval      st_dicint3 = %subst(dicfrase: 227: 29)
488400981021     c                   endif
488700981021      *
488800981021     C                   endsr
488801170127      *----------------------------------------------------*
488802170127      * Reperisco dichiarazione intento cliente
488803170127      *----------------------------------------------------*
488804170127     c     Sr_intento    begsr
488805170127
488806170127     c                   clear                   wfdicf00
488807170127      * se c'è esenzione passo come imponibile bolla a zero , quindi richiamo NewFattura
488808170127      * per recuperare i dati della dichiarazione intento
488809170127
488810170203     c                   if        wf_Wtasfei <> ' '
488811170127     c                   eval      imponibile = 0
488812170127     c                   else
488813170207     c                   eval      imponibile = (wf_wtasimv + dirfatt)
488814170127     c                   endif
488815170127      /free
488816170127       Intento_Init(kpjba);
488817170127
488818170127
488819170127       IF Intento_NewFattura( wsocieta : clnksc
488820170127                            : wrkdff : imponibile)= *ZERO;
488821170127         fatturato = Intento_GetImponibileFatturatoSenzaIvaAnno();
488822170127         importoDichiarazioniAnno = Intento_GetImportoDichiarazioniAnno();
488823170127         codiceIVA = Intento_GetCodiceIVA();
488824170127         frase = Intento_GetFraseFattura();
488825170127
488826170127       // scrivo file di lavoro dichiarazioni
488827170127         dicesito = 'S' ;
488828170203         diccdiva = codiceIVA ;
488829170127         dictotfat = fatturato;
488830170127         dicplafon = importoDichiarazioniAnno ;
488831170127         dicprgfat = fatturato  ;
488832170127         dicfrase  = frase ;
488833170127       ELSE ;
488834170127         dicesito = 'N' ;
488835170127       ENDIF;
488836170127
488837170127         dicksc = fatksc ;
488838170127         write wfdicf00 ;
488839170127
488840170127
488841170127       Intento_EndFattura();
488842170127
488843170127
488845170127       // Ho finito.
488846170127
488847170127       Intento_Finalize();
488848170127
488849170127      /end-free
488850170127     C                   endsr
488900050413      *----------------------------------------------------*
489000050413      * Reperisco numeratore
489100050413      *----------------------------------------------------*
489200050413     c     repnum        begsr
489300050413     c                   MOVE      wsocieta      XNMSOC            3
489400050413     c                   MOVE      *BLANKS       XNMUNI            8
489500050413     c                   MOVE      'CG'          XNMCTB            2
489600050413     c                   MOVE      *BLANKS       XNMKEY            8
489700050413     c                   MOVE      WSERIE        XNMSER            4
489800050413     c                   MOVE      *OFF          XNMCMT            1
489900050413     c                   MOVE      *On           XNMIVA            1
490000050413     c                   MOVE      *OFF          XNMREG            1
490100050413     c                   MOVE      '0'           XNMerr            1
490200050413      *
490300050413     c                   CALLB     'XNMR'        PNMR
490400050413      *
490500050413     c     xnmerr        ifne      '0'
490600050413     c                   seton                                        h5
490700050413     c                   end
490800981023      *
490900050413     c                   endsr
491000050413      *----------------------------------------------------*
491100050413      * Reperimento dati società
491200050413      *----------------------------------------------------*
491300050413     c     REPSOC        BEGSR
491400050413      *
491500050413     c                   CALL      'XSOC'
491600050413     c                   PARM                    TIPXSC            6
491700050413     c                   PARM      wsocieta      SOCXSC            3
491800050413     c                   PARM                    CDSXSC            9 0
491900050413     c                   PARM                    MODXSC            3
492000050413     c                   PARM      *blanks       RTNXSC            1
492100050413     c                   PARM                    XSOCDS
492200050413     c                   PARM                    KPJBA
492300050413      *
492400050413     c                   ENDSR
492500050413      *****************************************************************
492600050413     c     *INZSR        BEGSR
492700050413      *****************************************************************
492800050413     c     *ENTRY        PLIST
492900050413     c                   PARM                    KPJBA
493000050413     c                   MOVEL     *ZEROS        PARAM
493100050413     c                   MOVEL     KPJBU         PARAM
493200050413      *
493300050413     c                   Z-ADD     1             CODUT
493400050413     c                   CALL      'X§PARUT'
493500050413     c                   PARM                    UTEDSE
493600050413      * RICERCA CAPOCONTI
493700050413     c                   DO        50            X                 2 0
493800050413     c                   MOVE      TCU(X)        TCUDS
493900050413     c     F56           CABNE     'CG'          END1
494000050413     c     F4            COMP      '1'                                    21
494100050413     c     F4            COMP      '2'                                    22
494200050413     c     F4            COMP      '3'                                    23
494300050413     c     F4            COMP      '6'                                    27
494400050413      ** 1 CLIENTI   21
494500050413      ** 2 FORNITORI 22
494600050413      ** 3 AGENTI    23
494700050413     c     F3            COMP      '0'                                242425
494800050413     c     F3            COMP      'I'                                    26
494900050413      ** 0 ITALIA   25
495000050413      ** 1 ESTERO   24
495100050413      *  I CAPO CONTO IVA
495200050413     c   21
495300050413     cAN 24              Z-ADD     KCU(X)        KCE               4 0
495400050413     c   21
495500050413     cAN 25              Z-ADD     KCU(X)        KCI               4 0
495600050413     c   22
495700050413     cAN 24              Z-ADD     KCU(X)        KFE               4 0
495800050413     c   22
495900050413     cAN 25              Z-ADD     KCU(X)        KFI               4 0
496000050413     c   23
496100050413     cAN 24              Z-ADD     KCU(X)        KAE               4 0
496200050413     c   23
496300050413     cAN 25              Z-ADD     KCU(X)        KAI               4 0
496400050413     c   26              Z-ADD     KCU(X)        KIVA              4 0
496500050413     c   27              Z-ADD     KCU(X)        KBNA              4 0
496600050413     c     END1          TAG
496700050413     c                   END
496800990715     c                   setoff                                       212223
496900990715     c                   setoff                                       242526
497000990715     c                   setoff                                       27
497100981023      *parametri per richiamo numeratori
497200050413     c     PNMR          PLIST
497300050413     c                   PARM                    XNMRIC            1
497400050413     c                   PARM                    XNMSOC
497500050413     c                   PARM                    XNMUNI
497600050413     c                   PARM                    XNMCTB
497700050413     c                   PARM                    XNMKEY
497800050413     c                   PARM                    XNMSER
497900050413     c                   PARM                    XNMCMT            1
498000050413     c                   PARM                    XNMDAT
498100050413     c                   PARM                    XNMIVA            1
498200050413     c                   PARM                    XNMREG            1
498300050413     c                   PARM                    XNMERR            1
498400050413     c                   PARM                    XNMNUM            9 0
498500050413     c                   PARM                    XNMAUT            1
498600050413     c                   PARM                    XNMDAG                         opzionale
498700050413      *---------- RICERCA DITTA :
498800050413     c                   MOVEL     'SOC001'      TIPXSC
498900050413     c                   MOVEL     *BLANK        SOCXSC
499000050413     c                   EXSR      REPSOC
499100050413     c     RTNXSC        IFNE      '1'
499200050413     c                   MOVEL     XSOCDS        SOC001
499300050413     c                   MOVEL     xscrgs        RSUT             20
499400050413     c                   end
499500050413     c                   movel     wsocieta      WWWSoc
499600050413     c                   movel     'C     '      $kcc
499700050413     c                   movel     *blanks       $ksc
499800050413     c                   callb     'XCAPCLIFOR'
499900050413     c                   parm                    WWWSoc
500000050413     c                   parm                    $kcc
500100050413     c                   parm                    $ksc
500200050413     c                   Z-ADD     1             CODUT
500300050413      * IMPOSTAZIONE CAMPI
500400050413     c                   CLEAR                   SAVLNP
500500050413     c                   CLEAR                   SAVLNA
500600050413     C                   move      *zero         FATKCC
500700050413     C                   move      *zero         FATKSC
500800050413     C                   move      *zero         FATSC1
500900050413     C                   move      *blank        FATVIA
501000050413     C                   move      *blank        FATcit
501100050413     C                   move      *zero         FATcap
501200050413     C                   move      *blank        FATprv
501300050413     C                   move      *blank        FATbna
501400050413     C                   move      *blank        FATsbl
501500050413      *  KLIST PER ACCESSO FILES
501600050413     c     KTA7          KLIST
501700050413     c                   KFLD                    TASAAS
501800050413     c                   KFLD                    TASLNP
501900050413     c                   KFLD                    TASNRS
502000050413     c                   KFLD                    TASNSP
502100050413     c                   KFLD                    TASTBL
502200050413      *  file di lavoro wfasc01l
502300050411     c     Kasc          klist
502400050411     c                   kfld                    tasaas
502500050411     c                   kfld                    taslnp
502600050411     c                   kfld                    tasnrs
502700050411     c                   kfld                    tasnsp
502800050413      *
502900050413     c     KTAA          KLIST
503000050413     c                   KFLD                    TASAAS
503100050413     c                   KFLD                    TASLNP
503200050413     c                   KFLD                    TASNRS
503300050413     c                   KFLD                    TASNSP
503400050413     c                   KFLD                    §trc
503500050413      *
503600050413     c     KTA4          KLIST
503700050413     c                   KFLD                    TASAAS
503800050413     c                   KFLD                    TASLNP
503900050413     c                   KFLD                    TASNRS
504000050413     c                   KFLD                    TASNSP
504100050413     c                   KFLD                    §4trc
504200050929      * Chiave file eventi FNEVB
504300080619     c     Kevb4         Klist
504400080619     c                   Kfld                    Tasaas
504500080619     c                   Kfld                    Taslnp
504600080619     c                   Kfld                    Tasnrs
504700080619     c                   Kfld                    Tasnsp
504800080619     c                   Kfld                    Wcev
504900050929      *
505000050413     c     KCSB          KLIST
505100050413     c                   KFLD                    TASAAS
505200050413     c                   KFLD                    TASLNP
505300050413     c                   KFLD                    TASNRS
505400050413     c                   KFLD                    TASNSP
505500941108     C** RICERCA NUMERI FATTURA
505600050411      ** Accesso TITAS per stampa e contabilizzazione
505700050429     c     Ktas30        klist
505800050411     c                   kfld                    wf_tasAAS
505900050411     c                   kfld                    wf_tasLNP
506000050411     c                   kfld                    wf_tasNRS
506100050411     c                   kfld                    wf_tasNSP
506200050429     c                   kfld                    wf_tasTBL
506300050413      ** ACCESSO TABEL
506400050413     c     KTABC         KLIST
506500050413     c                   KFLD                    CODUT
506600050413     c                   KFLD                    COD
506700050413     c                   KFLD                    KEY
506800050331
506900050331     C     KTAB          KLIST
507000050331     C                   KFLD                    CODUT
507100050331     C                   KFLD                    COD
507200981021     C     Keycli        klist
507300981021     C                   kfld                    wsocieta
507400981021     C                   kfld                    ktasksc
507500981021     C                   kfld                    k£fil
507600981021     C                   kfld                    k£lin
507700050420     C     Keybol        klist
507800050420     C                   kfld                    wsocieta
507900050421     C                   kfld                    ktasbsc
508000050420     C                   kfld                    k£fil
508100050420     C                   kfld                    k£lin
508200981021     C     Keyfat        klist
508300981021     C                   kfld                    wsocieta
508400981023     C                   kfld                    clnclifatt
508500981021     C                   kfld                    k£fil
508600981021     C                   kfld                    k£lin
508700981021     C     Keyind        KLIST
508800981021     C                   KFLD                    sogsogg
508900981021     C                   KFLD                    tpin
509000981021     C                   KFLD                    cdin
509100981021     c                   move      *blank        tpin
509200981021     c                   move      *blank        cdin
509300981021     C     Keybac        klist
509400981021     C                   kfld                    wsocieta
509500981021     C                   kfld                    clnkcc
509600981021     C                   kfld                    clnksc
509700050509     C     Keyclp        klist
509800050509     C                   kfld                    codut
509900050509     C                   kfld                    clpkcc
510000050509     C                   kfld                    clpksc
510100981021     C     Kcba          klist
510200981021     C                   kfld                    wsocieta
510300981021     C                   kfld                    wcbacli           1
510400981021     C                   kfld                    wccblank          6
510500981021     c                   kfld                    clnksc
510600981021     c                   kfld                    entita            8
510700981021     c                   kfld                    wappog            2
510800981021     c                   move      *blank        entita
510900981021     c                   move      *blank        wccblank
511000981021     c                   move      *blank        wappog
511100981021     c                   move      'C'           wcbacli
511600981021     C     Keycdp        klist
511700981021     C                   kfld                    wsocieta
511800981021     C                   kfld                    fatcdp
511900981022     C     Kriv          klist
512000981022     C                   kfld                    wsocieta
512100981022     C                   kfld                    wtpreg            1
512200981022     C                   kfld                    rivaliq
512300981022     C                   kfld                    rivrifiva
512400030204
512500030204     c     kFiar5        Klist
512600030204     c                   Kfld                    TasAas
512700030204     c                   Kfld                    TasLnp
512800030204     c                   Kfld                    TasNrs
512900030204     c                   Kfld                    TasNsp
513000030204     c                   Kfld                    kAr5Trd
513100050413      ** ACCESSO ANAGRAFICHE CLIENTI
513200050331     C     KSPE          KLIST
513300050331     C                   KFLD                    SPWFLS
513400050331     C                   KFLD                    wCLPKSC           7 0
513500050331     C                   KFLD                    COMLSF            3
513600051110      * accesso al file luoghi con indirizzo mail
513700051110     c     Ksp2          Klist
513800051110     c                   kfld                    specli
513900051110     c                   kfld                    specod
514000051110     c                   kfld                    comtpe
514100051118      *  file note tfntc01l
514200051118     C     Kntc          KLIST
514300051118     C                   KFLD                    Kapl
514400051118     C                   KFLD                    Knk1
514500051118     C                   KFLD                    Knk2
514600051118     C                   KFLD                    Ktnt
514700060502      *  file immagine bolle tassate
514800060502     c     Ktai          Klist
514900060502     c                   Kfld                    wf_TASaas
515000060502     c                   Kfld                    wf_TASlnp
515100060502     c                   Kfld                    wf_TASnrs
515200060502     c                   Kfld                    wf_TASnsp
515300060502     c                   Kfld                    wf_TAStbl
515500051118      *
515600050413      *----------------------------------------------------
515700941108     C                   MOVE      KNSIF         COMOD             4
515800941108     C                   MOVEL     COMOD         KCDSI             3
515900941108     C                   Z-ADD     66            CR                2 0
516000050413      **
516100050413      ** CARICAMENTO TABELLE
516200941108     C                   EXSR      CARTAB
516300050413      ** RIBALTA DATE DA FILTRO
516400941108     C                   EXSR      RIBDAT
516500050413     C** CARICAMENTO NUMERI DI FATTURA
516600050413      *  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
516700941108     C                   EXSR      CARNFT
516800050413
516900050413     c                   TIME                    W0140            14 0
517000050413      * UDATE IN GGMMAAAA
517100980105     C                   MOVE      W0140         WDTGIO            8 0
517200050413      *
517300050413      * UDATE IN AAAAMMGG
517400981023     C                   move      WDTGIO        dataeur
517500981023     C                   MOVE      dataeur       dataiso
517600981023     C                   MOVE      dataiso       DATEU             8 0
517700050329      ** Carica limiti di fatturazione per divisa di fatturazione (EUR)
517800050329     c                   exsr      cardiv
517900050421      * apro i file di stampa
518000060228     c                   open      prtf198
518100050421     c                   open      qsysprt
518200050421     c                   open      sysprt
518300060227      *
518400060227
518500060227      * Se sono in ambiente buono - GAITRAAZM
518600060227     c                   If        knsif = 'GAITRA201 '
518700061012     c                   Eval      wlibc= 'GAITRAAZM'
518800060227      *
518900060227     c                   Else
519000060227     c                   Eval      wlibc= 'GAITRAFIL'
519100060227     c                   EndIf
519200060227      * file di lavoro wfmtc se sono in filiale duplico in QTEMP
519300061011     c                   clear                   comman
519400061011     c                   movel(p)  cmdc(1)       comman
519500061012     c                   Eval      %Subst(comman:15:9) = wlibc
519600061011     c                   call      'QCMDEXC'
519700061011     c                   parm                    comman
519800061011     c                   parm                    lenght
519900061011
520000060227     c                   clear                   comman
520100061011     c                   movel(p)  cmdc(2)       comman
520200061012     c                   Eval      %Subst(comman:34:9) = wlibc
520300060227     c                   call      'QCMDEXC'
520400060227     c                   parm                    comman
520500060227     c                   parm                    lenght
520600061011
520700061011     c                   clear                   comman
520800061011     c                   movel(p)  cmdc(3)       comman
520900061011     c                   call      'QCMDEXC'
521000061011     c                   parm                    comman
521100061011     c                   parm                    lenght
521200061011
521300060228     c                   open      wfmtc00f
521400150909      * file di lavoro wfvpd se sono in filiale duplico in QTEMP
521500150909     c                   clear                   comman
521600150909     c                   movel(p)  cmdc(4)       comman
521700150909     c                   Eval      %Subst(comman:15:9) = wlibc
521800150909     c                   call      'QCMDEXC'
521900150909     c                   parm                    comman
522000150909     c                   parm                    lenght
522100150909
522200150909     c                   clear                   comman
522300150909     c                   movel(p)  cmdc(5)       comman
522400150909     c                   call      'QCMDEXC'
522500150909     c                   parm                    comman
522600150909     c                   parm                    lenght
522700150909
522800160426     c                   open      wfvpd10f
522900050413      **
523000100708      ** Se fatt mensile, pulisco file distinte
523100100708     c                   if        vidtft=4
523200100708     c     *start        setll     wfdss00f
523300100708     c                   read      wfdss00f
523400100708     c                   dow       not %eof(wfdss00f)
523500100708     c                   delete    wfdss000
523600100708     c                   read      wfdss00f
523700100708     c                   enddo
523800100708     c                   endif
523900100708     c
524000941108     C                   ENDSR
524100050413      **----------------------------------------------------
524200050413      ** AGGIORNAMENTO FILE CON ULTIMO NUMERO/DATA FATTURA *
524300050413      **----------------------------------------------------
524400050413     c     AGGNUM        BEGSR
524500981027     c                   move      '5'           xnmric
524600050413      ** FATTURA INIZIO MESE
524700050413      **  SIA PER FATTURAZ. MENSILE CHE NON
524800050413     c                   MOVEl     §QCRIV        wserie                         REG.IVA VEND.
524900050413     c                   MOVE      §QCFIV        wserie                         LIBRO IVA VEND.
525000050413     c     NFTIM         IFNE      wnftim
525100981026     c   20              move      dtdfi8        xnmdat
525200981026     c  n20              move      dtdft8        xnmdat
525300981027     c   20              move      dtdfi8        xnmdag
525400981027     c  n20              move      dtdft8        xnmdag
525500981023     c                   z-add     nftim         xnmnum
525600981023     c                   exsr      repnum
525700050413     c                   ENDif
525800050413      **
525900050413      ** FATTURA FINE MESE
526000981023     C     *IN20         IFEQ      *ON
526100981026     c                   move      dtdff8        xnmdat
526200981027     c                   move      dtdff8        xnmdag
526300050413     c                   MOVE      §QCFI2        wserie                         LIBRO IVA VEND.
526400050413     c     NFTFM         IFNE      wnftfm
526500050413     c                   Z-ADD     NFTFM         xnmnum
526600050413     c                   exsr      repnum
526700050413     c                   END
526800050413     c                   END
526900050413     c                   ENDSR
527000941108     C*****************************************************
527100941108     C** CARICAMENTO TABELLE
527200941108     C*****************************************************
527300050413     c     CARTAB        BEGSR
527400050413      ** CARICAMENTO TIPI TARIFFA
527500941113     C                   EXSR      CARTR
527600050413      ** CARICAMENTO SIGLE VARIE
527700941108     C                   EXSR      CAR$2
527800050413      ** CARICAMENTO DATI VARI TASSAZIONE
527900941108     C                   EXSR      CARQT
528000050413      ** CARICAMENTO TIPO BOLLA
528100941108     C                   EXSR      CARTB
528200050413      ** ESENZIONI IVA
528300941108     C                   EXSR      CAREI
528400050413      ** CODICI TASSAZIONE
528500941108     C                   EXSR      CARCT
528600050413      ** CONTI CONTABILITA
528700941108     C                   EXSR      CARCC
528800050413      ** TABELLA DATI VARI CONTABILITA'
528900941108     C                   EXSR      CARQC
529000050413      ** CARICA TIPI SERVIZIO
529100941113     C                   EXSR      CAR5E
529200050413      ** CARICA CONTI CONTAB. ARROTONDAMENTI CAMBI
529300050413     C                   EXSR      CARARR
529400050413      ** CARICA NAZIONI E LE SUE ESENZIONI IVA
529500050413     C                   EXSR      CARNAZ
529600050413      ** CARICo codici bolla che prevedono solo una varia
529700020416     C                   EXSR      CARcbo
529800120917      ** CARICO clienti che vogliono le stampe separate per CUP e CIG
529900120917     C                   EXSR      CARCUP
530000150113      ** CARICAMENTO particolarità consegna
530100150113     C                   EXSR      CAR7R
530200050413
530300941108     C                   ENDSR
530400050413      ******************************************************
530500050413      ** CARICAMENTO TIPI TARIFFA
530600050413      ******************************************************
530700050413     c     CARTR         BEGSR
530800050413
530900050413     c                   DO        50            A
531000050413     c     A             OCCUR     DSTR
531100050413     c                   CLEAR                   DSTR
531200050413     c                   END
531300050413      **
531400050413     c                   MOVEL     'TR'          COD
531500050413     c                   Z-ADD     0             K                 5 0
531600050413     c     KTAB          SETLL     TABEL                                  07
531700050413     c  N07              SETON                                          H7
531800050413     c   H7              GOTO      FINE
531900050413     c                   DO        *HIVAL        A
532000050413     c     KTAB          READE     TABEL                                  07
532100050413     c   07              GOTO      TAB1
532200050413     c     TBLKEY        IFNE      *BLANKS
532300050413     c                   ADD       1             K
532400050413     c                   MOVEL     TBLKEY        CTR(K)
532500050413     c     K             OCCUR     DSTR
532600050413     c                   MOVEL     TBLUNI        DSTR
532700050413     c                   END
532800050413     c                   END
532900050413
533000050413     c     TAB1          ENDSR
533100941113     C**********************************************************
533200980224     C** CARICAMENTO TIPI SERVIZIO DA STAMPARE
533300980224     C*  QUELLI CHE HANNO UNA TARIFFA PARTICOLARE COLLEGATA
533400941113     C**********************************************************
533500050413     c     CAR5E         BEGSR
533600050413      ** PULIZIA DS
533700050413     c                   MOVEL     '5E'          COD
533800050413     c                   Z-ADD     0             K
533900050413     c     KTAB          SETLL     TABEL                                  07
534000050413     c     KTAB          READE     TABEL                                  07
534100050413    1c     *IN07         DOWEQ     *OFF
534200050413    2c     TBLKEY        IFNE      *BLANKS
534300050413     c                   MOVEL     TBLUNI        DS5E
534400050413    3c     §5EFTC        IFNE      ' '
534500050413     c                   ADD       1             K
534600050413     c                   MOVEL     TBLKEY        TS(K)
534700050413    3c                   ENDIF
534800050413    2c                   ENDIF
534900050413     c     KTAB          READE     TABEL                                  07
535000050413    1c                   ENDDO
535100050413     c                   ENDSR
535200020416     C**********************************************************
535300020416     C** CARICAMENTO codici bolla che prevedono di tassare solo 1 bolla
535400020416     C*   e che permettono imponibile =0
535500020416     C**********************************************************
535600050413     c     CARCBO        BEGSR
535700050413      ** PULIZIA DS
535800020416     C                   MOVEL     '3A'          COD
535900020416     C                   Z-ADD     0             K
536000020416     C                   Z-ADD     0             KK                3 0
536100151008     C                   Z-ADD     0             KK2
536200020416     C     KTAB          SETLL     TABEL                                  07
536300020416     C     KTAB          READE     TABEL                                  07
536400020416    1C     *IN07         DOWEQ     *OFF
536500020416    2C     TBLKEY        IFNE      *BLANKS
536600020416     C                   MOVEL     TBLUNI        DS3a
536700020416    3C     §3aim0        IFeq      'S'
536800020416     C                   ADD       1             K
536900020416     C                   MOVEL     TBLKEY        im0(k)
537000020416    3C                   ENDIF
537100020416     c     §3asva        ifne      *blanks
537200020416     C                   ADD       1             Kk
537300020416     C                   MOVEL     TBLKEY        cbo(kk)
537400020416     C                   MOVEL     §3asva        cbv(kk)
537500020416    3c                   endif
537600151008     c     §3atb2        ifne      *blanks
537700151008     C                   ADD       1             Kk2
537800151008     C                   MOVEL     TBLKEY        cbo2(kk2)
537900151008     C                   MOVEL     §3atb2        TBL2(kk2)
538000151008    3c                   endif
538100050413      **
538200020416    2C                   ENDIF
538300020416     C     KTAB          READE     TABEL                                  07
538400020416    1C                   ENDDO
538500020416     C                   ENDSR
538600050413      ********************************************************
538700050413      ** CARICAMENTO SIGLE VARIE
538800050413      ********************************************************
538900050413     c     CAR$2         BEGSR
539000050413     c                   CLEAR                   DS$2
539100050413     c                   MOVEL     '$2'          COD
539200050413     c                   MOVEL     *BLANKS       KEY
539300050413     c                   MOVEL     '1'           KEY
539400050413     c     Ktabc         CHAIN     TABEL                              07
539500050413     c   07              SETON                                          H7
539600050413     c   H7              GOTO      FINE
539700050413     c                   MOVEL     TBLUNI        DS$2
539800050413     c                   ENDSR
539900941108     C****************************************************
540000941108     C** CARICAMENTO DATI VARI CONTABILITA'
540100941108     C****************************************************
540200050413     c     CARQC         BEGSR
540300050413     c                   CLEAR                   DSQC1
540400050413     c                   CLEAR                   DSQC2
540500050413     c                   MOVEL     'QC'          COD
540600050413     c                   MOVEL     *BLANKS       KEY
540700050413     c                   MOVEL     '2'           KEY
540800050413     c     KTABC         CHAIN     TABEL                              07
540900050413     c   07              SETON                                        H7
541000050413     c   H7              GOTO      FINE
541100050413     c                   MOVEL     TBLUNI        DSQC2
541200050413     c                   MOVEL     *BLANKS       KEY
541300050413     c                   MOVEL     '1'           KEY
541400050413     c     KTABC         CHAIN     TABEL                              07
541500050413     c   07              SETON                                        H7
541600050413     c   H7              GOTO      FINE
541700050413     c                   MOVEL     TBLUNI        DSQC1
541800050413      **
541900050413     c                   ENDSR
542000050413      ****************************************************
542100050413      ** CARICAMENTO DATI VARI TASSAZIONE
542200050413      ****************************************************
542300050413     c     CARQT         BEGSR
542400050413     c                   CLEAR                   DSQT1
542500050413     c                   MOVEL     'QT'          COD
542600050413     c                   MOVEL     *BLANKS       KEY
542700050413     c                   MOVEL     '1'           KEY
542800050413     c     KTABC         CHAIN     TABEL                              07
542900050413     c   07              SETON                                        H7
543000050413     c   H7              GOTO      FINE
543100050413     c                   MOVEL     TBLUNI        DSQT1
543200050413     c                   ENDSR
543300050329      ****************************************************
543400050329      ** Caricamento importi standard moneta
543500050329      ****************************************************
543600050329     c     Cardiv        Begsr
543700050329      *
543800050329     c                   Clear                   Dsbs02
543900050329
544000050329     c                   Movel     'L'           t02tla
544100050329     c                   Movel     'C'           t02mod
544200050329     c                   Movel     Knsif         t02sif
544300050329     c                   Movel     'GEI'         t02cod
544400050329     c                   Movel     Viddiv        t02ke1
544500050329
544600050329     c                   call      'TIBS02R'
544700050329     c                   parm                    kpjba
544800050415     c                   parm                    dsbs02
544900050329
545000050418     c                   movel     t02uni        dgei
545100050418
545200050329     c                   endsr
545300001024     C*******************************************************************
545400001024     C** CARICO TABELLA DELLE NAZIONI E LE RELATIVE ESENZIONI IVA
545500001024     C*******************************************************************
545600050413     c     CARNAZ        BEGSR
545700050413      * PULISCO LE SCHIERE
545800001024     C                   CLEAR                   NAZE
545900001024     C                   CLEAR                   ESEN
546000001024     C**
546100001024     C                   MOVEL     '15'          COD
546200001024     C                   Z-ADD     0             YY
546300001024     C     KTAB          SETLL     TABEL                                  07
546400001024     C  N07              SETON                                          H7
546500001024     C   H7              GOTO      FINE
546600001024     C                   DO        *HIVAL
546700001024     C     KTAB          READE     TABEL                                  07
546800001024     C   07              LEAVE
546900001024     C                   ADD       1             YY
547000001024     C                   MOVEL     TBLKEY        NAZE(YY)
547100001024     C                   MOVEL     TBLUNI        DS15
547200001024     c                   movel     §15CEI        ESEN(YY)
547300001024     C*
547400001024     C                   ENDDO
547500050413      *
547600001024     C                   ENDSR
547700120917     C**********************************************************
547800120917     C** CARICAMENTO SCHIERA CLIENTI CHE VOGLIONO FATTURE SEPARATE PER CUP E CIG
547900120917     C**********************************************************
548000120917     C     CARCUP        BEGSR
548100120917
548200120917     C                   CLEAR                   KX
548300120917
548400120917     C                   MOVEL     'CLI'         KTBE
548500120917     C     KTBE          SETLL     TNTBE01L
548600120917     C                   DO        *HIVAL
548700120917     C     KTBE          READE     TNTBE01L
548800120917
548900120917     C                   IF        %EOF(TNTBE01L)
549000120917     C                   LEAVE
549100120917     C                   ENDIF
549200120917
549300120917     C                   IF        TBEATB = ' '
549400120917     C                   EVAL      DCLI = TBEUNI
549500120917     C                   IF        §CLIFTS =  'S'
549600120917     C                   IF        KX <= 998
549700120917     C                   ADD       1             KX
549800120917     C                   movel     tbeke1        skcup(KX)
549900120917     c                   else
550000120917     c                   leave
550100120917     C                   ENDIF
550200120917     C                   ENDIF
550300120917     C                   ENDIF
550400120917
550500120917     C                   ENDDO
550600120917
550700120917     C                   ENDSR
550800150112     C**********************************************************
550900150112     C** CARICAMENTO SCHIERA PARTICOLARITA' CONSEGNE
551000150112     C**********************************************************
551100150112     C     CAR7R         BEGSR
551200150112
551300150112      /free
551400150112       //?Carico part.consegna con abilitazione PinCode
551500150112         COD = '7R';
551600150112         clear kk;
551700150112         setll (CODUT:COD) TABEL00F;
551800150112         reade (CODUT:COD) TABEL00F;
551900150112         DOW  not %eof(TABEL00F);
552000150112           IF  TBLflg = *blanks;
552100150112             ds7R = TBLuni;
552200150112             IF  §7Rpincode = 'S';
552300150112               kk += 1;
552400150112               skGMA(kk) = TBLkey;
552500150112             ENDIF;
552600150112           ENDIF;
552700150112           reade (CODUT:COD) TABEL00F;
552800150112         ENDDO;
552900150112      /end-free
553000150112      *
553100150112
553200150112     C                   ENDSR
553300000104     C****************************************************
553400000104     C** CONVERSIONE IMPORTI IN UNA MONETA DIVERSA
553500000104     C****************************************************
553600000104     C     CNVEUR        BEGSR
553700000107      * conversione in moneta di conto e non
553800000107     C                   Movel     div2          DivMC0202
553900000107     C                   Movel     div1          Divisa0202
554000000107     C                   Move      *Loval        DtRif0202
554100000107     C                   Move      *On           NoCtrScos
554200030701     C                   if        fatdft > 0
554300000107     C                   move      fatdft        dtrif0202
554400030701     C                   endif
554500000107     c**
554600000107     C                   CallP     X0202TestC    (PJX00202DS       :
554700000107     C                                            %Size(PJX00202DS):
554800000107     C                                            *Off             :
554900000107     C                                            EsitoA           :
555000000107     C                                            NoCtrScos        :
555100000107     C                                            *Omit            :
555200000107     C                                            %Size(ANDVS00F)  :
555300000107     C                                            %Size(ANDVX00F)  :
555400000107     C                                            ANDVS00F         :
555500000107     C                                            ANDVX00F          )
555600000107     C                   endsr
555700000104      *
555800000111     C****************************************************
555900000111     C** CARICAMENTO TABELLA ARR
556000000111     C****************************************************
556100050413     c     CARARR        BEGSR
556200050413      *  CARICO DS ARROTONDAMENTI ATTIVI
556300000111     C                   CLEAR                   DSBS02
556400000111     C                   CLEAR                   DARR
556500000111      *
556600000111     C                   MOVEL     'L'           T02TLA
556700000111     C                   MOVEL     'C'           T02MOD
556800000111     C                   MOVEL     KNSIF         T02SIF
556900000111     C                   MOVEL     'ARR'         T02COD
557000000111     C                   MOVEL     'ATTIVO '     T02KE1
557100000111      *
557200000111     C                   CALL      'TIBS02R'
557300000111     C                   PARM                    KPJBA
557400000111     C                   PARM                    DSBS02
557500000111      *
557600000111     C     T02ERR        IFEQ      *BLANKS
557700000111     C     1             OCCUR     DARR
557800000111     C                   MOVEL     T02UNI        DARR
557900000111     C                   ENDIF
558000000111     C*  CARICO DS ARROTONDAMENTI ATTIVI
558100000111     C                   CLEAR                   DSBS02
558200000111      *
558300000111     C                   MOVEL     'L'           T02TLA
558400000111     C                   MOVEL     'C'           T02MOD
558500000111     C                   MOVEL     KNSIF         T02SIF
558600000111     C                   MOVEL     'ARR'         T02COD
558700000111     C                   MOVEL     'PASSIVO'     T02KE1
558800000111      *
558900000111     C                   CALL      'TIBS02R'
559000000111     C                   PARM                    KPJBA
559100000111     C                   PARM                    DSBS02
559200000111      *
559300000111     C     T02ERR        IFEQ      *BLANKS
559400000111     C     2             OCCUR     DARR
559500000111     C                   MOVEL     T02UNI        DARR
559600000111     C                   ENDIF
559700000111      *
559800000111     C                   ENDSR
559900941108     C******************************************************
560000941108     C** RIBALTAMENTO DATE DA FILTRO
560100941108     C******************************************************
560200050413     c     RIBDAT        BEGSR
560300050413      ** DATA FATTURA AMG INIZIO MESE
560400981023     C                   move      VIDDFI        dataeur
560500981023     C                   move      dataeur       dataiso
560600981023     C                   MOVE      dataiso       DTDFI8            8 0
560700050413      **DATA DECORRENZA AMG FINE MESE
560800981023     C                   move      VIDDFF        dataeur
560900981023     C                   move      dataeur       dataiso
561000981023     C                   MOVE      dataiso       DTDFF8            8 0
561100050413      ** DATA FATTURA AMG PER FATTURAZIONE SETTIMANALE O QUINDICINALE
561200981023     C                   move      VIDDFT        dataeur
561300981023     C                   move      dataeur       dataiso
561400981023     C                   MOVE      dataiso       DTDFT8            8 0
561500050413      **DATA SPEDIZIONE AMG
561600981023     C                   move      VIDDSP        dataeur
561700981023     C                   move      dataeur       dataiso
561800981023     C                   MOVE      dataiso       DTDSP             8 0
561900050413      * VEDO SE SIT RATTA DI FATTURAZIONE MENSILE O MENO
562000971023     C     VIDTFT        IFEQ      4
562100971023     C                   SETON                                        20
562200971023     C                   ELSE
562300971023     C                   SETOFF                                       20
562400971023     C                   ENDIF
562500941108     C                   ENDSR
562600941108     C*********************************************************
562700941108     C** CARICAMENTO DS MULTIPLA  CONTI CONTABILITA
562800941108     C*********************************************************
562900050413     c     CARCC         BEGSR
563000150113     c                   DO        90            A
563100050413     c     A             OCCUR     DSCC
563200050413     c                   CLEAR                   DSCC
563300050413     c                   END
563400050413      ** PULIZIA DS
563500050413     c                   MOVEL     'CC'          COD
563600050413      ** ACCESSO TABEL X CODICE
563700050413     c     KTAB          SETLL     TABEL                                  07
563800050413     c  N07              SETON                                          H7
563900050413     c   H7              GOTO      FINE
564000050413     c                   Z-ADD     0             K
564100050413     c                   DO        *HIVAL        A
564200050413     c     KTAB          READE     TABEL                                  07
564300050413     c   07              GOTO      ENDCC
564400050413     c     TBLKEY        IFNE      *BLANKS
564500050413     c                   ADD       1             K
564600050413     c     K             OCCUR     DSCC
564700050413     c                   MOVEL     TBLKEY        CC(K)
564800050413     c                   MOVEL     TBLUNI        DSCC
564900050413     c                   END
565000050413     c                   END
565100050413     c     ENDCC         TAG
565200050413      ** tabella cb estensione per descrizione  varie fedex
565300150113     c                   DO        90            A
565400050413     c     A             OCCUR     DSCB
565500050413     c                   CLEAR                   DSCB
565600050413     c                   END
565700050413      ** PULIZIA DS
565800050413     c                   MOVEL     'CB'          COD
565900050413      ** ACCESSO TABEL X CODICE
566000050413     c     KTAB          SETLL     TABEL                                  07
566100050413     c  N07              goto      endcb
566200050413     c                   Z-ADD     0             K
566300050413     c                   DO        *HIVAL        A
566400050413     c     KTAB          READE     TABEL                                  07
566500050413     c   07              GOTO      ENDCB
566600050413     c     TBLKEY        IFNE      *BLANKS
566700050413     c                   ADD       1             K
566800050413     c     K             OCCUR     DSCB
566900050413     c                   MOVEL     TBLKEY        CB(K)
567000050413     c                   MOVEL     TBLUNI        DSCB
567100050413     c                   END
567200050413     c                   END
567300050413     c     ENDCB         TAG
567400050413     c                   ENDSR
567500941108     C**********************************************************
567600941108     C** CARICAMENTO DS MULTIPLA  ESENZIONI IVA
567700941108     C**********************************************************
567800941108     C     CAREI         BEGSR
567900050413     c                   DO        100           A
568000050413     c     A             OCCUR     DSEI
568100050413     c                   CLEAR                   DSEI
568200050413     c                   END
568300050413      ** PULIZIA DS
568400050413     c                   MOVEL     'EI'          COD
568500050413     c     KTAB          SETLL     TABEL                                  07
568600050413     c  N07              SETON                                          H7
568700050413     c   H7              GOTO      FINE
568800050413     c                   Z-ADD     0             K
568900050413     c                   DO        *HIVAL        A
569000050413     c     KTAB          READE     TABEL                                  07
569100050413     c   07              GOTO      ENDEI
569200050413     c     TBLKEY        IFNE      *BLANKS
569300050413     c                   ADD       1             K
569400050413     c     K             OCCUR     DSEI
569500050413     c                   MOVEL     TBLKEY        CEI(K)
569600050413     c                   MOVEL     TBLUNI        DSEI
569700050413     c                   END
569800050413     c                   END
569900050413     c     ENDEI         TAG
570000050413     c                   ENDSR
570100050331      **********************************************************
570200050331      ** Caricamento DS Multipla Codici Tassazione
570300050331      **********************************************************
570400050331     c     Carct         Begsr
570500050331      ** pulizia
570600050331     c                   do        600           A
570700050331     c     a             occur     dsct
570800050331     c                   clear                   dsct
570900050331     c                   enddo
571000050331      ** Carico la schiera
571100050331     c                   movel     'CT'          cod
571200050331     c     ktab          setll     tabel                                  07
571300050331     c  N07              seton                                          H7
571400050331     c   H7              goto      fine
571500050331     c                   z-add     0             k
571600050331     c                   do        *hival        a
571700050331     c     ktab          reade     tabel00f
571800050418     c                   If        %eof(tabel00f)
571900050418     c                   leave
572000050418     c                   endif
572100050418     c                   If        tblkey <> *blanks
572200050331     c                   add       1             k
572300050331     c                   movel     tblkey        cts(k)
572400050331     c     k             occur     dsct
572500050331     c                   movel     tbluni        dsct
572600050331     c                   endif
572700050331     c                   enddo
572800050331
572900050331     c                   endsr
573000981023     C**----------------------------------------
573100981023     C**    CARICAMENTO NUMERI DI FATTURA
573200981023     C**----------------------------------------
573300050413     c     CARNFT        BEGSR
573400050413      **
573500050413      ** PER FATTURE FINE MESE
573600981027     C                   MOVE      '1'           XNMRIC            1
573700981023     C                   MOVEl     §QCRIV        wserie            4            REGISTRO VEND.
573800981023     C     *IN20         IFEQ      *ON
573900981026     C                   MOVEL     DTDFF8        xnmdat                         ANNO FATTURA
574000981027     C                   MOVEL     DTDFF8        xnmdag                         ANNO FATTURA
574100981023     C                   MOVE      §QCFI2        wserie                         LIBRO IVA FT.A.
574200981023     c                   exsr      repnum
574300981023     C                   z-add     xnmnum        nftfm
574400981023     C                   z-add     xnmnum        wnftfm            9 0
574500981023     C                   ENDIF
574600050413      ** PER FATTURE INIZIO MESE
574700050413      **  E FATTURAZIONE NON MENSILE
574800981026     C   20              MOVEL     DTDFI8        xnmdat                         ANNO FATTURA
574900981026     C  N20              MOVEL     DTDFT8        xnmdat                         ANNO FATTURA
575000981027     C   20              MOVEL     DTDFI8        xnmdag                         ANNO FATTURA
575100981027     C  N20              MOVEL     DTDFT8        xnmdag                         ANNO FATTURA
575200981023     C                   MOVE      §QCFIV        wserie                         LIBRO IVA VEND.
575300981023     c                   exsr      repnum
575400981023     C                   Z-ADD     xnmnum        NFTIM
575500981023     C                   z-add     xnmnum        wnftim            9 0
575600981023     C                   ENDSR
575700050413      *****************************************************
575800050413      ** CARICAMENTO DS TIPO BOLLA
575900050413      *****************************************************
576000050413     c     CARTB         BEGSR
576100050413     c                   DO        20            A
576200050413     c     A             OCCUR     DSTB
576300050413     c                   CLEAR                   DSTB
576400050413     c                   END
576500050413      ** PULIZIA DS
576600050413     c                   MOVEL     'TB'          COD
576700050413     c     KTAB          SETLL     TABEL                                  07
576800050413     c  N07              SETON                                          H7
576900050413     c   H7              GOTO      FINE
577000050413     c                   Z-ADD     0             K
577100050413     c                   DO        *HIVAL        A
577200050413     c     KTAB          READE     TABEL                                  07
577300050413     c   07              GOTO      ENDTB
577400050413     c     TBLKEY        IFNE      *BLANKS
577500050413     c                   ADD       1             K
577600050413     c                   MOVEL     TBLKEY        TB(K)
577700050413     c     K             OCCUR     DSTB
577800050413     c                   MOVEL     TBLUNI        DSTB
577900050413     c                   END
578000050413     c                   END
578100050413     c     ENDTB         TAG
578200050413     c                   ENDSR
578300981022      *----------------------------------------------------
578400981022     C     primanota     BEGSR
578500981022     C*----------------------------------------------------
578600981022     C*
578700981022     C* contabilizzazione batch
578800981022     c                   reset                   ndc071ds
578900981022     C                   MOVE      wsocieta      SOCieta071
579000981022     C                   MOVE      KNRAZ         rifint071
579100981022     c                   move      *loval        dtregda071
579200981022     c                   move      *hival        dtrega071
579300981022     c                   move      *loval        subnumd071
579400981022     c                   move      *hival        subnuma071
579500990506     C                   MOVE      *off          solcont071                     contab.
579600990506     C***                MOVE      *on           solcont071                     solo stampa
579700981022    >C                   MOVE      *off          interat071
579800981022     C                   move      *off          err071
579900981022     c                   move      *off          control071
580000981022     C                   MOVEL     NDC071DS      kpjbu
580100981022     C*
580200981022     C                   CALL      'NDC071C'
580300981022     C                   parm                    kpjba
580400981022     C                   movel     kpjbu         ndc071ds
580500981022     C*
580600981022     C                   ENDSR
580700060228
580800060228      ****************************************************
580900060228      ** Richiamo programma stampa manca tariffa
581000060228      ****************************************************
581100060228     c     mancatar      Begsr
581200060228      *
581300060228     c                   clear                   tnsb44ds
581400060228
581500060228     c                   move      00010101      D44dsd
581600060228     c                   move      dtdsp         D44dsa
581700060228      * creazione file
581800060228     c                   eval      d44crf = 'S'
581900060228      *
582000060228     c                   movel     d44dsa        com8a
582100060301     c                   eval      D44mbr = 'F'+ com8a
582200060228      * stampa imponibile
582300060228     c                   eval      d44fsi = 'N'
582400060228      * stampa separata per P.O.
582500060228     c                   eval      d44spp = 'S'
582600060228      * stampa unica
582700060228     c                   eval      d44sun = 'N'
582800060228      * coda di stampa
582900060228     c                   eval      d44out = 'O'
583000060228      * totali per area
583100060228     c                   eval      d44tot = 'S'
583200060228      * stampante totali per area
583300060228     c                   eval      d44Tou = 'T'
583400060228
583500060228
583600060228     c                   close     wfmtc00f
583700160426     c                   close     wfvpd10f
583800060228
583900060228      * LANCIO
584000060228     c                   movel     tnsb44ds      Kpjbu
584100060228
584200060228     c                   call      'TNSB46R'
584300060228     c                   parm                    kpjba
584400060228      *
584500060228     c                   endsr
584600060228
584700941121     O******************************************************************
584800060228     OPRTF198   E            TESTCON          01
584900050413     O                       RSUT                20
585000060228     O                                           83 'CONTROLLO FATTURAZIONE AL'
585100060228     O                       VIDDFI             + 3 '  /  /    '
585200050413     O                                           96 'TNSF07R'
585300050413     O                       UDATE              112 '  /  /  '
585400050413     O          E            TESTCON          04
585500050413     O                                              '-------------------------'
585600050413     O                                              '-------------------------'
585700050413     O                                              '-------------------------'
585800050413     O                                              '-------------------------'
585900050413     O                                              '-------------------------'
586000050413     O                                              '-------'
586100050413     O          E            TESTCON          05
586200050413     O                                              '                         '
586300050413     O                                              '             '
586400050413     O                                              'ERRORI CONTABILIZZAZIONE'
586500050413     O                                              ' FATTURE'
586600050413     O                                              '                         '
586700050413     O                                              '             '
586800050413     O          E            TESTCON          06
586900050413     O                                              '-------------------------'
587000050413     O                                              '-------------------------'
587100050413     O                                              '-------------------------'
587200050413     O                                              '-------------------------'
587300050413     O                                              '-------------------------'
587400050413     O                                              '-------'
587500050413     O          E            DETFAT      2
587600050413     O                                              'Fattura n° '
587700050413     O                       fatnft        z   +  2
587800050413     O                                              'del '
587900050413     O                       stadft            +  2 '  /  /  '
588000050413     o                                              'Cliente '
588100050413     O                       tasksc            +  2
588200050413     O                                         +  2 'più di tre righe di totale'
588300050413     O          E            DETcon      2
588400050413     O                                              'Spedizione '
588500050413     O                       tasaas            +  2
588600050413     O                                              '--'
588700050413     O                       taslnp
588800050413     O                                              '--'
588900050413     O                       tasnrs
589000050413     O                                              '--'
589100050413     O                       tasnsp
589200050413     O
589300050413     O                       tasmsg            +  5
589400000000     ODISSPE    E            HDGDIS           02
589500000000     O                       RSUT                20
589600060228     O                                           83 'ELENCO DISTINTE SPEDIZIONI'
589700060228     O                                           +3 'Al '
589800060228     O                       VIDDFI              +1 '  /  /    '
589900000000     O                       UDATE              112 '  /  /  '
590000000000     O          E            HDGDIS           04 06
590100000724     O                                           24 '---CLIENTE--------------'
590200000000     O                                           48 '------------------------'
590300000724     O                                           72 '------------------------'
590400010924     O                                           98 'IMPONIBILE--DIVISA--N.SPED'
590500010927     O                                          104 'IZIONI'
590600000000     O          E            DETDIS      2
590700000724     O                       BOLKSC
590800000724     O                       FATKSC            +  1
590900000000     O                       FATRA1            +  1
591000050413     O                       TIMD          2     84
591100010924     O                       VIDDIV              88
591200010924      * numero totale spedizioni in distinta
591300010927     O                       numspe        z    104
591400001024     o* totali distinta spedizione
591500001024     O          E            statds      2
591600001024     O                                           72 '            ------------'
591700010926     o                                           88 '----------------'
591800001027     O          E            statds      2
591900050413     o                       tdspd         2     84
592000010924     o                       viddiv              88
592100960904     OFTWCODIM  E            HCODI            04
592200960927     O                       STADFT              68 '  /  /  '
592300960927     O                       FATNFT        Z     85
592400130125     O                                         +  1 '('
592500130125     O                       FATFIV            +  0
592600130125     O                                         +  0 ')'
592700960927     O                       PAGE          Z    101
592800960927     O                       WLBETI             113
592900960904     O          E            HCODI            17
593000960927     O                                              'CODICE CLIENTE:'
593100960904     O                       FATKSC            +  2
593200960904     O                                         +  4 '('
593300960904     O                       ASFAT             +  0
593400960904     O                                         +  0 ')'
593500080919     O                                         +  1 '('
593600080919     O                       FATFIV            +  0
593700080919     O                                         +  0 ')'
593800960904     O          E            HCODI            19
593900960927     O                                              'Per informazioni'
594000900926     O                                         +  1 'rivolgersi presso:'
594100960904     O          E            HCODI            21
594200960927     O                       PDFI
594300960927     O                       FATRA1             103
594400960904     O          E            HCODI            22
594500960927     O                       PVIA
594600960904     O          E            HCODI            23
594700960927     O                       PCAP
594800900704     O                       PLOC              +  2
594900900704     O                       PPRV              +  2
595000960927A    O                       FATVIA              85
595100960912     O          E            HCODI            24
595200960927     O                                              'Telef.:'
595300960912     O                       PTEL              +  2
595400960912     O          E   N01      HCODI            25
595500960927     O                                              'Fax   :'
595600960912     O                       PFAX              +  2
595700960927     O                       FATCAP              60
595800960905     O                       FATCIT            +  2
595900960905     O                       FATPRV            +  2
596000960905     O          E    01      HCODI            25
596100960927     O                                              'Fax   :'
596200960912     O                       PFAX              +  2
596300960927     O                       FATCAE              64
596400960905     O                       FATCIT            +  2
596500960905     O                       FATPRV            +  2
596600960905     O                       FATSTA            +  2
596700060324     O          E            HCODI            26
596800060324     O                       mittmail
596900960912     O          E            HCODI            27
597000960927     O                                              'Cod.Cli.Bollettazione:'
597100950901     O                       BOLKSC            +  1
597200960905     O                                         +  4 '('
597300960905     O                       ASBOL             +  0
597400960905     O                                         +  0 ')'
597500070508     O          E   N01      HCODI            28
597600061108     O                                              'C.F.  : '
597700061108     O                       FATCDF            +  1
597800960912     O          E            HCODI            29
597900960927     O                                              'P.IVA : '
598000950901     O                       FATIVA            +  1
598100051110     O          E            HCODI            30
598200051110     O                       stmail1
598300051110     O          E            HCODI            31
598400051110     O                       stmail2
598500960912     O          E            HCODI            35 38
598600960927     O                       WFADES
598700960912     O                       FATBNA            + 12
598800960905     O** TESTATA DALLA 2 PAGINA
598900961028     O          E            HCODI2           08
599000960927     O                       STADFT              68 '  /  /  '
599100960927     O                       FATNFT        Z     85
599200130125     O                                         +  1 '('
599300130125     O                       FATFIV            +  0
599400130125     O                                         +  0 ')'
599500960927     O                       PAGE          Z    101
599600960927     O                       WLBETI             113
599700961028     O          E            HCODI2           10 14
599800970108     O                       FATKSC            + 61
599900961029     O                       FATRA2            +  1
600000960905     O**
600100000110     O          E            H2CODI      1
600200000110     O                       bolra1              48
600300000110     O          E            H2CODI      1
600400000110     O                       bolvia              30
600500000110     O              n15      bolcap              36
600600000110     O               15      bolcae              40
600700000110     O                       bolcit              71
600800000110     O                       bolprv              74
600900050421      ** bsc codice bollettazione di raggruppamento
601000050421     O          E            db1ODI      1
601100050426     O                       wf_tasbsc     Z
601200050426     O                       bscra1            +  1
601300050421     O          E            db2ODI      1
601400050421     O                       bscvia              30
601500050421     O              n16      bsccap              36
601600050421     O               16      bsccae              40
601700050421     O                       bsccit              71
601800050421     O                       bscprv              74
601900000110     O          E            DCODI       1
602000050413     O                       ID(3)         4    113
602100000124     O                       S(3)               105
602200050413     O                       ID(2)         4    104
602300000124     O                       S(2)                96
602400050413     O                       ID(1)         4     95
602500000124     O                       S(1)                87
602600050413     O                       PRTPORD       4     86
602700960927     O                     09TASQFT        4     77
602800960927     O                    N09TASPVL        Z     77
602900020827     O                    N09STApkf        4     72
603000160427     O                  41N09STAVLF        2     65
603100960927     O                       TASNCL        Z     58
603200960927     O                       WCAP                53
603300960927     O                       RSC13               47
603400960927     O                       CIT3                33
603500960927     O                       CIT2                30
603600030512     O              n29      RMN9          Z     27
603700030512     O               29      RMa09               27
603800960927     O                       WTSP                18
603900960927     O                       MDSP                17
604000960927     O                       GDSP                15
604100960927     O                       TASNSP        Z     12
604200960927     O                       TASNRS        Z      5
604300960927     O                       TASLNP               3
604400941110     O          E            DCODI2      1
604500050413     O                       I2D(3)        4    113
604600000124     O                       S2(3)              105
604700050413     O                       I2D(2)        4    104
604800000124     O                       S2(2)               96
604900050413     O                       I2D(1)        4     95
605000000124     O                       S2(1)               87
605100020527     O               22                          50 'Rif:'
605200020527     O               22      WMITalf             66
605300020527     O               22                          19 'Mancato ritiro'
605400020527     O               22      WMITORI             45
605500020527     O              n22      WMITORI             30
605600000324     O               19                          42 'PARCEL :'
605700060703     O               19      WPARCEL             57
605800000110     O          E            DMIOI2      1
605900020527     O               22                          50 'Rif:'
606000020527     O               22      WMITalf             66
606100020527     O               22                          19 'Mancato ritiro'
606200020527     O               22      WMITORI             45
606300020527     O              N22      WMITORI             30
606400000324     O               19                          42 'PARCEL :'
606500060703     O               19      WPARCEL             57
606600030909     O          E            DCOOI2      1
606700030909     O                                        +   5 'Colli Originali :'
606800030909     O                       StpNcl        Z  +   1
606900030909     O          E            DDEII2      1
607000030909     O                       DesEsIva         +   5
607100110711     O          E            DCUPI2      1
607200110711     O                                        +   5 'CUP-CIG :'
607300110711     O                       StpCup           +   1
607400030923      * diritto di fatturazione VARIA D
607500030923     O          E            DDVDI2      1
607600050413     O                       variadd       4     95
607700030923     O                                           87 'D'
607800050422     O          E            tboDI0      1
607900050422     O          E            tboDI1      1
608000050422     o                                         +  0 'Tot.Cli.:'
608100050421     o                       tksc          z   +  1
608200050422     o                       trsc                54
608300060908     o                                           59 'Tot.'
608400050421     o                       timps         4     76
608500050421     o                                           84 'Franchi'
608600050421     o                       tfras         4     95
608700060908     o                                          102 'Asseg.'
608800050421     o                       tasss         4    113
608900050620
609000050620     O          E            tboDI1P     1
609100050620     o                                           29 '   Pag. da '
609200050620     o                       tpgi          z   +  0
609300050620     o                                         +  1 'a  '
609400050620     o                       tpgf          z   +  0
609500050620
609600050421     O          E            tboDi2      1
609700050421     o                                         +  0 'TOTALI   '
609800050421     O          E            tboDi3      1
609900050421     o                                           61 'Totale'
610000050421     o                       timpst        4     76
610100050421     o                       tfrast        4     95
610200050421     o                       tassst        4    113
610300941119     O          E            DCODI4      1
610400020320     O                       decleg
610500941118     O          E            DCODI4      1
610600960927     O                       LES(1)
610700941119     O                       LIN(1)            +  0
610800941118     O                       LED(1)            +  0
610900941119     O                       LES(2)            +  2
611000941119     O                       LIN(2)            +  0
611100941118     O                       LED(2)            +  0
611200941119     O                       LES(3)            +  2
611300941119     O                       LIN(3)            +  0
611400941118     O                       LED(3)            +  0
611500941119     O                       LES(4)            +  2
611600941119     O                       LIN(4)            +  0
611700941118     O                       LED(4)            +  0
611800941119     O                       LES(5)            +  2
611900941119     O                       LIN(5)            +  0
612000941118     O                       LED(5)            +  0
612100941119     O                       LES(6)            +  2
612200941119     O                       LIN(6)            +  0
612300941118     O                       LED(6)            +  0
612400941119     O                       LES(7)            +  2
612500941119     O                       LIN(7)            +  0
612600941118     O                       LED(7)            +  0
612700941119     O                       LES(8)            +  2
612800941119     O                       LIN(8)            +  0
612900941118     O                       LED(8)            +  0
613000960906     O          E            DCODI5      1
613100960927     O                       LES(9)
613200960906     O                       LIN(9)            +  0
613300960906     O                       LED(9)            +  0
613400960906     O                       LES(10)           +  2
613500960906     O                       LIN(10)           +  0
613600960906     O                       LED(10)           +  0
613700960906     O                       LES(11)           +  2
613800941119     O                       LIN(11)           +  0
613900941118     O                       LED(11)           +  0
614000941119     O                       LES(12)           +  2
614100941119     O                       LIN(12)           +  0
614200941118     O                       LED(12)           +  0
614300941119     O                       LES(13)           +  2
614400941119     O                       LIN(13)           +  0
614500941118     O                       LED(13)           +  0
614600941119     O                       LES(14)           +  2
614700941119     O                       LIN(14)           +  0
614800941118     O                       LED(14)           +  0
614900941119     O                       LES(15)           +  2
615000941119     O                       LIN(15)           +  0
615100941118     O                       LED(15)           +  0
615200941119     O                       LES(16)           +  2
615300941119     O                       LIN(16)           +  0
615400941118     O                       LED(16)           +  0
615500960906     O          E            DCODI6      1
615600960927     O                       LES(17)
615700941119     O                       LIN(17)           +  0
615800941118     O                       LED(17)           +  0
615900941119     O                       LES(18)           +  2
616000941119     O                       LIN(18)           +  0
616100941118     O                       LED(18)           +  0
616200941119     O                       LES(19)           +  2
616300941119     O                       LIN(19)           +  0
616400941118     O                       LED(19)           +  0
616500941119     O                       LES(20)           +  2
616600941119     O                       LIN(20)           +  0
616700941118     O                       LED(20)           +  0
616800021104     O          E    45      TCODI4      2
616801170202     O                       ST_dicInt1         113
616900170202     O**                                         82 'Come da Vs.dichiarazione'
617000170202     O**                                        107 'Prot.N.       per l''anno'
617100170202     O**                     FATNPR        Z     96
617200170202     O**                     FATAPR             113
617201170202     O          E    45 46   TCODI4      1
617202170202     O                       ST_dicInt2         113
617203170202     O          E    45 47   TCODI4      1
617204170202     O                       ST_dicInt3         113
617300021104     O          E    11      tCODI4      1
617400021104     O                       staese              85
617500160623     O****      E            tCODI5      1
617600160623     O****                   stavdl              85
617700960905     O          E            TCODI            90
617800941120     O          E            TCODI2      1
617900050413     O               89      IVD(A)        4     73
618000960927     O               89      PERIVA        4     61
618100130117     O              N89      DES(A)              78
618200050413     O                       IMD(A)        4     55
618300130618     O*******                VARIED        4     40
618400130618     O*******                PRTTPID       4     25
618500941110     O          E            TCODI3      0
618600050413     O                       FATTOTD       4    112
618700050413     O                       FATBOLD       4     97
618800000126     O          E    60      TCODI3      1
618900000124     O                       VIDDIV               6
619000000124     O                                            8 ':'
619100000126     O          E   n60      TCODI3      0
619200000126     O                       VIDDIV               6
619300000126     O                                            8 ':'
619400960905     O          E            RCODI            92
619500960927     O                                          110 'Riporto'
619600951031     O          E            RCODI2      0
619700960927     O                                          113 'SEGUE TOTALI'
619800960906     OFTWCODFM  E            HCODF            04
619900960927     O                       STADFT              68 '  /  /  '
620000960927     O                       FATNFT        Z     85
620100130125     O                                         +  1 '('
620200130125     O                       FATFIV            +  0
620300130125     O                                         +  0 ')'
620400960927     O                       PAGE          Z    101
620500960927     O                       WLBETI             113
620600960906     O          E            HCODF            17
620700960927     O                                              'CODICE CLIENTE:'
620800960906     O                       FATKSC            +  2
620900960906     O                                         +  4 '('
621000960906     O                       ASFAT             +  0
621100960906     O                                         +  0 ')'
621200080919     O                                         +  1 '('
621300080919     O                       FATFIV            +  0
621400080919     O                                         +  0 ')'
621500960906     O          E            HCODF            19
621600960927     O                                              'Per informazioni'
621700960906     O                                         +  1 'rivolgersi presso:'
621800960906     O          E            HCODF            21
621900960927     O                       PDFI
622000960930     O                       FATRA1             103
622100960906     O          E            HCODF            22
622200960927     O                       PVIA
622300960906     O          E            HCODF            23
622400960927     O                       PCAP
622500960906     O                       PLOC              +  2
622600960906     O                       PPRV              +  2
622700960930     O                       FATVIA              85
622800960912     O          E            HCODF            24
622900960927     O                                              'Telef.:'
623000960906     O                       PTEL              +  2
623100960912     O          E   N01      HCODF            25
623200960927     O                                              'Fax   :'
623300960912     O                       PFAX              +  2
623400960930     O                       FATCAP              60
623500960906     O                       FATCIT            +  2
623600960906     O                       FATPRV            +  2
623700960906     O          E    01      HCODF            25
623800960927     O                                              'Fax   :'
623900960912     O                       PFAX              +  2
624000960930     O                       FATCAE              64
624100960906     O                       FATCIT            +  2
624200960906     O                       FATPRV            +  2
624300960906     O                       FATSTA            +  2
624400060324     O          E            HCODF            26
624500060324     O                       mittmail
624600960912     O          E            HCODF            27
624700960927     O                                              'Cod.Cli.Bollettazione:'
624800960906     O                       BOLKSC            +  1
624900960906     O                                         +  4 '('
625000960906     O                       ASBOL             +  0
625100960906     O                                         +  0 ')'
625200070508     O          E   N01      HCODF            28
625300061108     O                                              'C.F.  : '
625400061108     O                       FATCDF            +  1
625500960912     O          E            HCODF            29
625600960927     O                                              'P.IVA : '
625700960906     O                       FATIVA            +  1
625800051110     O          E            HCODF            30
625900051110     O                       stmail1
626000051110     O          E            HCODF            31
626100051110     O                       stmail2
626200960912     O          E            HCODF            35 38
626300960927     O                       WFADES
626400960912     O                       FATBNA            + 12
626500960906     O** TESTATA DALLA 2 PAGINA
626600961029     O          E            HCODF2           08
626700960927     O                       STADFT              68 '  /  /  '
626800960927     O                       FATNFT        Z     85
626900130125     O                                         +  1 '('
627000130125     O                       FATFIV            +  0
627100130125     O                                         +  0 ')'
627200960927     O                       PAGE          Z    101
627300960927     O                       WLBETI             113
627400961029     O          E            HCODF2           10 14
627500970108     O                       FATKSC            + 61
627600961029     O                       FATRA2            +  1
627700000110     O**
627800000110     O          E            H2CODF      1
627900000110     O                       bolra1              48
628000000110     O          E            H2CODF      1
628100000110     O                       bolvia              30
628200000110     O              n15      bolcap              36
628300000110     O               15      bolcae              40
628400000110     O                       bolcit              71
628500000110     O                       bolprv              74
628600050421      ** bsc codice bollettazione di raggruppamento
628700050421     O          E            db1ODf      1
628800050426     O                       wf_tasbsc     Z
628900050426     O                       bscra1            +  1
629000050421     O          E            db2ODf      1
629100050421     O                       bscvia              30
629200050421     O              n16      bsccap              36
629300050421     O               16      bsccae              40
629400050421     O                       bsccit              71
629500050421     O                       bscprv              74
629600000110     O**
629700960906     O          E            DCODF       1
629800050413     O                       ID(3)         4    113
629900000124     O                       S(3)               105
630000050413     O                       ID(2)         4    104
630100000124     O                       S(2)                96
630200050413     O                       ID(1)         4     95
630300000124     O                       S(1)                87
630400050413     O                       PRTPORD       4     86
630500960927     O                     09TASQFT        4     77
630600960927     O                    N09TASPVL        Z     77
630700020827     O                    N09STApkf        4     72
630800160427     O                  41N09STAVLF        2     65
630900960927     O                       TASNCL        Z     58
631000960927     O                       WCAP                53
631100960927     O                       RSC13               47
631200960927     O                       CIT3                33
631300960927     O                       CIT2                30
631400030512     O              n29      RMN9          Z     27
631500030512     O               29      RMa09               27
631600960927     O                       WTSP                18
631700960927     O                       MDSP                17
631800960927     O                       GDSP                15
631900960927     O                       TASNSP        Z     12
632000960927     O                       TASNRS        Z      5
632100960927     O                       TASLNP               3
632200960906     O          E            DCODF2      1
632300050413     O                       I2D(3)        4    113
632400000124     O                       S2(3)              105
632500050413     O                       I2D(2)        4    104
632600000124     O                       S2(2)               96
632700050413     O                       I2D(1)        4     95
632800000124     O                       S2(1)               87
632900020527     O               22                          50 'Rif:'
633000020527     O               22      WMITalf             66
633100020527     O               22                          19 'Mancato ritiro'
633200020527     O               22      WMITORI             45
633300020527     O              N22      WMITORI             30
633400000324     O               19                          42 'PARCEL :'
633500060703     O               19      WPARCEL             57
633600000110     O          E            DMIOF2      1
633700020527     O               22                          50 'Rif:'
633800020527     O               22      WMITalf             66
633900020527     O               22                          19 'Mancato ritiro'
634000020527     O               22      WMITORI             45
634100020527     O              N22      WMITORI             30
634200000324     O               19                          42 'PARCEL :'
634300060703     O               19      WPARCEL             57
634400030909     O          E            DCOOF2      1
634500030909     O                                        +   5 'Colli Originali :'
634600030909     O                       StpNcl        Z  +   1
634700110711     O          E            DCUPF2      1
634800110711     O                                        +   5 'CUP-CIG :'
634900110711     O                       StpCup           +   1
635000030909     O          E            DDEIF2      1
635100030909     O                       DesEsIva         +   5
635200030923      * diritto di fatturazione VARIA D
635300030923     O          E            DDVDF2      1
635400050413     O                       variadd       4     95
635500030923     O                                           87 'D'
635600050422     O          E            tboDf0      1
635700050422     O          E            tboDf1      1
635800050422     o                                         +  0 'Tot.Cli.:'
635900050421     o                       tksc          z   +  1
636000050422     o                       trsc                54
636100060908     o                                           59 'Tot.'
636200060908     o                       timps         4     76
636300060908     o                                           84 'Franchi'
636400060908     o                       tfras         4     95
636500060908     o                                          102 'Asseg.'
636600060908     o                       tasss         4    113
636700050620
636800050620     O          E            tboDf1P     1
636900050620     o                                           29 '   Pag. da '
637000050620     o                       tpgi          z   +  0
637100050620     o                                         +  1 'a  '
637200050620     o                       tpgf          z   +  0
637300050620
637400050421     O          E            tboDf2      1
637500050421     o                                         +  0 'TOTALI   '
637600050421     O          E            tboDf3      1
637700050421     o                                           61 'Totale'
637800050421     o                       timpst        4     76
637900050421     o                       tfrast        4     95
638000050421     o                       tassst        4    113
638100960906     O          E            DCODF4      1
638200020320     O                       decleg
638300960906     O          E            DCODF4      1
638400960927     O                       LES(1)
638500960906     O                       LIN(1)            +  0
638600960906     O                       LED(1)            +  0
638700960906     O                       LES(2)            +  2
638800960906     O                       LIN(2)            +  0
638900960906     O                       LED(2)            +  0
639000960906     O                       LES(3)            +  2
639100960906     O                       LIN(3)            +  0
639200960906     O                       LED(3)            +  0
639300960906     O                       LES(4)            +  2
639400960906     O                       LIN(4)            +  0
639500960906     O                       LED(4)            +  0
639600960906     O                       LES(5)            +  2
639700960906     O                       LIN(5)            +  0
639800960906     O                       LED(5)            +  0
639900960906     O                       LES(6)            +  2
640000960906     O                       LIN(6)            +  0
640100960906     O                       LED(6)            +  0
640200960906     O                       LES(7)            +  2
640300960906     O                       LIN(7)            +  0
640400960906     O                       LED(7)            +  0
640500960906     O                       LES(8)            +  2
640600960906     O                       LIN(8)            +  0
640700960906     O                       LED(8)            +  0
640800960906     O          E            DCODF5      1
640900960927     O                       LES(9)
641000960906     O                       LIN(9)            +  0
641100960906     O                       LED(9)            +  0
641200960906     O                       LES(10)           +  2
641300960906     O                       LIN(10)           +  0
641400960906     O                       LED(10)           +  0
641500960906     O                       LES(11)           +  2
641600960906     O                       LIN(11)           +  0
641700960906     O                       LED(11)           +  0
641800960906     O                       LES(12)           +  2
641900960906     O                       LIN(12)           +  0
642000960906     O                       LED(12)           +  0
642100960906     O                       LES(13)           +  2
642200960906     O                       LIN(13)           +  0
642300960906     O                       LED(13)           +  0
642400960906     O                       LES(14)           +  2
642500960906     O                       LIN(14)           +  0
642600960906     O                       LED(14)           +  0
642700960906     O                       LES(15)           +  2
642800960906     O                       LIN(15)           +  0
642900960906     O                       LED(15)           +  0
643000960906     O                       LES(16)           +  2
643100960906     O                       LIN(16)           +  0
643200960906     O                       LED(16)           +  0
643300960906     O          E            DCODF6      1
643400960927     O                       LES(17)
643500960906     O                       LIN(17)           +  0
643600960906     O                       LED(17)           +  0
643700960906     O                       LES(18)           +  2
643800960906     O                       LIN(18)           +  0
643900960906     O                       LED(18)           +  0
644000960906     O                       LES(19)           +  2
644100960906     O                       LIN(19)           +  0
644200960906     O                       LED(19)           +  0
644300960906     O                       LES(20)           +  2
644400960906     O                       LIN(20)           +  0
644500960906     O                       LED(20)           +  0
644600021104     O          E    45      TCODF4      2
644601170202     O                       St_DicInt1         113
644700170202     O**                                         82 'Come da Vs.dichiarazione'
644800170202     O**                                        107 'Prot.N.       per l''anno'
644900170202     O**                     FATNPR        Z     96
645000170202     O**                     FATAPR             113
645001170202     O          E    45 46   TCODF4      1
645002170202     O                       St_DicInt2         113
645003170202     O          E    45 47   TCODF4      1
645004170202     O                       St_DicInt3         113
645100021104     O          E    11      tCODF4      1
645200021104     O                       staese              85
645300160623     O****      E            tCODf5      1
645400160623     O****                   stavdl              85
645500960906     O          E            TCODF            90
645600960906     O          E            TCODF2      1
645700050413     O               89      IVD(A)        4     73
645800960927     O               89      PERIVA        4     61
645900130117     O              N89      DES(A)              78
646000050413     O                       IMD(A)        4     55
646100130618     O*******                VARIED        4     40
646200130618     O*******                PRTTPID       4     25
646300960906     O          E            TCODF3      0
646400050413     O                       FATTOTD       4    112
646500050413     O                       FATBOLD       4     97
646600000126     O          E    60      TCODF3      1
646700000124     O                       VIDDIV               6
646800000124     O                                            8 ':'
646900000126     O          E   n60      TCODF3      0
647000000126     O                       VIDDIV               6
647100000126     O                                            8 ':'
647200960906     O          E            RCODF            92
647300960927     O                                          110 'Riporto'
647400960906     O          E            RCODF2      0
647500960927     O                                          113 'SEGUE TOTALI'
647600960906     O**
647700960906     OFTWCODDS  E            HCODD            04
647800960927     O                       STADFT              68 '  /  /  '
647900960927     O                       PAGE          Z    101
648000960927     O                       WLBETI             113
648100960917     O          E            HCODD            05
648200960927     O                                           92 'DISTINTA SPEDIZIONI'
648300960906     O          E            HCODD            17
648400960927     O                                              'CODICE CLIENTE:'
648500960906     O                       FATKSC            +  2
648600960906     O                                         +  4 '('
648700960906     O                       ASFAT             +  0
648800960906     O                                         +  0 ')'
648900080919     O                                         +  1 '('
649000080919     O                       FATFIV            +  0
649100080919     O                                         +  0 ')'
649200960906     O          E            HCODD            19
649300960927     O                                              'Per informazioni'
649400960906     O                                         +  1 'rivolgersi presso:'
649500960906     O          E            HCODD            21
649600960927     O                       PDFI
649700960930     O                       FATRA1             103
649800960906     O          E            HCODD            22
649900960927     O                       PVIA
650000960906     O          E            HCODD            23
650100960927     O                       PCAP
650200960906     O                       PLOC              +  2
650300960906     O                       PPRV              +  2
650400960930     O                       FATVIA              85
650500960912     O          E            HCODD            24
650600960927     O                                              'Telef.:'
650700960906     O                       PTEL              +  2
650800960912     O          E   N01      HCODD            25
650900960927     O                                              'Fax   :'
651000960912     O                       PFAX              +  2
651100960930     O                       FATCAP              60
651200960906     O                       FATCIT            +  2
651300960906     O                       FATPRV            +  2
651400960906     O          E    01      HCODD            25
651500960927     O                                              'Fax   :'
651600960912     O                       PFAX              +  2
651700960930     O                       FATCAE              64
651800960906     O                       FATCIT            +  2
651900960906     O                       FATPRV            +  2
652000960906     O                       FATSTA            +  2
652100060406     O          E            HCODD            26
652200060324     O                       mittmail
652300960912     O          E            HCODD            27
652400960927     O                                              'Cod.Cli.Bollettazione:'
652500960906     O                       BOLKSC            +  1
652600960906     O                                         +  4 '('
652700960906     O                       ASBOL             +  0
652800960906     O                                         +  0 ')'
652900070508     O          E   N01      HCODD            28
653000061108     O                                              'C.F.  : '
653100061108     O                       FATCDF            +  1
653200960912     O          E            HCODD            29
653300960927     O                                              'P.IVA : '
653400960906     O                       FATIVA            +  1
653500051110     O          E            HCODD            30
653600080512     O*****                  stmail1                                            tolta mail
653700051110     O          E            HCODD            31
653800080512     O*****                  stmail2                                            tolta mail
653900960912     O          E            HCODD            35 38
654000960927     O                       WFADES
654100960912     O                       FATBNA            + 12
654200960906     O** TESTATA DALLA 2 PAGINA
654300960917     O          E            HCODD2           08
654400960927     O                       STADFT              68 '  /  /  '
654500960927     O                       PAGE          Z    101
654600960927     O                       WLBETI             113
654700961029     O          E            HCODD2           09
654800960927     O                                           92 'DISTINTA SPEDIZIONI'
654900961029     O          E            HCODD2           10 14
655000970108     O                       FATKSC            + 61
655100961029     O                       FATRA2            +  1
655200000110     O**
655300000110     O          E            H2CODD      1
655400000110     O                       bolra1              48
655500000110     O          E            H2CODD      1
655600000110     O                       bolvia              30
655700000110     O              n15      bolcap              36
655800000110     O               15      bolcae              40
655900000110     O                       bolcit              71
656000000110     O                       bolprv              74
656100050421      ** bsc codice bollettazione di raggruppamento
656200050421     O          E            db1ODd      1
656300050426     O                       wf_tasbsc     Z
656400050426     O                       bscra1            +  1
656500050421     O          E            db2ODd      1
656600050421     O                       bscvia              30
656700050421     O              n16      bsccap              36
656800050421     O               16      bsccae              40
656900050421     O                       bsccit              71
657000050421     O                       bscprv              74
657100000110     O**
657200960906     O**
657300960906     O          E            DCODD       1
657400050413     O                       ID(3)         4    113
657500000124     O                       S(3)               105
657600050413     O                       ID(2)         4    104
657700000124     O                       S(2)                96
657800050413     O                       ID(1)         4     95
657900000124     O                       S(1)                87
658000050413     O                       PRTPORD       4     86
658100960927     O                     09TASQFT        4     77
658200960927     O                    N09TASPVL        Z     77
658300020827     O                    N09STApkf        4     72
658400160427     O                  41N09STAVLF        2     65
658500960927     O                       TASNCL        Z     58
658600960927     O                       WCAP                53
658700960927     O                       RSC13               47
658800960927     O                       CIT3                33
658900960927     O                       CIT2                30
659000030512     O              n29      RMN9          Z     27
659100030512     O               29      RMa09               27
659200960927     O                       WTSP                18
659300960927     O                       MDSP                17
659400960927     O                       GDSP                15
659500960927     O                       TASNSP        Z     12
659600960927     O                       TASNRS        Z      5
659700960927     O                       TASLNP               3
659800960906     O          E            DCODD2      1
659900050413     O                       I2D(3)        4    113
660000000124     O                       S2(3)              105
660100050413     O                       I2D(2)        4    104
660200000124     O                       S2(2)               96
660300050413     O                       I2D(1)        4     95
660400000124     O                       S2(1)               87
660500020527     O               22                          50 'Rif:'
660600020527     O               22      WMITalf             66
660700020527     O               22                          19 'Mancato ritiro'
660800020527     O               22      WMITORI             45
660900020527     O              N22      WMITORI             30
661000060727     O               19                          42 'PARCEL :'
661100060727     O               19      WPARCEL             57
661200000110     O          E            DMIOD2      1
661300020527     O               22                          50 'Rif:'
661400020527     O               22      WMITalf             66
661500020527     O               22                          19 'Mancato ritiro'
661600020527     O               22      WMITORI             45
661700020527     O              N22      WMITORI             30
661800060727     O               19                          42 'PARCEL :'
661900060727     O               19      WPARCEL             57
662000030909     O          E            DCOOD2      1
662100030909     O                                        +   5 'Colli Originali :'
662200030909     O                       StpNcl        Z  +   1
662300110711     O          E            DCUPD2      1
662400110711     O                                        +   5 'CUP-CIG :'
662500110711     O                       StpCup           +   1
662600030909     O          E            DDEID2      1
662700030909     O                       DesEsIva         +   5
662800030923      * diritto di fatturazione VARIA D
662900030923     O          E            DDVDD2      1
663000050413     O                       variadd       4     95
663100030923     O                                           87 'D'
663200050422     O          E            tboDd0      1
663300050422     O          E            tboDd1      1
663400050422     o                                         +  0 'Tot.Cli.:'
663500050421     o                       tksc          z   +  1
663600050422     o                       trsc                54
663700060908     o                                           59 'Tot.'
663800060908     o                       timps         4     76
663900060908     o                                           84 'Franchi'
664000060908     o                       tfras         4     95
664100060908     o                                          102 'Asseg.'
664200060908     o                       tasss         4    113
664300050620
664400050620     O          E            tboDd1P     1
664500050620     o                                           29 '   Pag. da '
664600050620     o                       tpgi          z   +  0
664700050620     o                                         +  1 'a  '
664800050620     o                       tpgf          z   +  0
664900050620
665000050421     O          E            tboDd2      1
665100050421     o                                         +  0 'TOTALI   '
665200050421     O          E            tboDd3      1
665300050421     o                                           61 'Totale'
665400050421     o                       timpst        4     76
665500050421     o                       tfrast        4     95
665600050421     o                       tassst        4    113
665700960906     O          E            DCODD4      1
665800020320     O                       decleg
665900960906     O          E            DCODD4      1
666000960927     O                       LES(1)
666100960906     O                       LIN(1)            +  0
666200960906     O                       LED(1)            +  0
666300960906     O                       LES(2)            +  2
666400960906     O                       LIN(2)            +  0
666500960906     O                       LED(2)            +  0
666600960906     O                       LES(3)            +  2
666700960906     O                       LIN(3)            +  0
666800960906     O                       LED(3)            +  0
666900960906     O                       LES(4)            +  2
667000960906     O                       LIN(4)            +  0
667100960906     O                       LED(4)            +  0
667200960906     O                       LES(5)            +  2
667300960906     O                       LIN(5)            +  0
667400960906     O                       LED(5)            +  0
667500960906     O                       LES(6)            +  2
667600960906     O                       LIN(6)            +  0
667700960906     O                       LED(6)            +  0
667800960906     O                       LES(7)            +  2
667900960906     O                       LIN(7)            +  0
668000960906     O                       LED(7)            +  0
668100960906     O                       LES(8)            +  2
668200960906     O                       LIN(8)            +  0
668300960906     O                       LED(8)            +  0
668400960906     O          E            DCODD5      1
668500960927     O                       LES(9)
668600960906     O                       LIN(9)            +  0
668700960906     O                       LED(9)            +  0
668800960906     O                       LES(10)           +  2
668900960906     O                       LIN(10)           +  0
669000960906     O                       LED(10)           +  0
669100960906     O                       LES(11)           +  2
669200960906     O                       LIN(11)           +  0
669300960906     O                       LED(11)           +  0
669400960906     O                       LES(12)           +  2
669500960906     O                       LIN(12)           +  0
669600960906     O                       LED(12)           +  0
669700960906     O                       LES(13)           +  2
669800960906     O                       LIN(13)           +  0
669900960906     O                       LED(13)           +  0
670000960906     O                       LES(14)           +  2
670100960906     O                       LIN(14)           +  0
670200960906     O                       LED(14)           +  0
670300960906     O                       LES(15)           +  2
670400960906     O                       LIN(15)           +  0
670500960906     O                       LED(15)           +  0
670600960906     O                       LES(16)           +  2
670700960906     O                       LIN(16)           +  0
670800960906     O                       LED(16)           +  0
670900960906     O          E            DCODD6      1
671000960927     O                       LES(17)
671100960906     O                       LIN(17)           +  0
671200960906     O                       LED(17)           +  0
671300960906     O                       LES(18)           +  2
671400960906     O                       LIN(18)           +  0
671500960906     O                       LED(18)           +  0
671600960906     O                       LES(19)           +  2
671700960906     O                       LIN(19)           +  0
671800960906     O                       LED(19)           +  0
671900960906     O                       LES(20)           +  2
672000960906     O                       LIN(20)           +  0
672100960906     O                       LED(20)           +  0
672200021104     O          E    45      TCODD4      2
672201170202     O                       St_DicInt1         113
672300170202     O**                                         82 'Come da Vs.dichiarazione'
672400170202     O**                                        107 'Prot.N.       per l''anno'
672500170202     O**                     FATNPR        Z     96
672600170202     O**                     FATAPR             113
672601170202     O          E    45 46   TCODD4      1
672602170202     O                       St_DicInt2         113
672603170202     O          E    45 47   TCODD4      1
672604170202     O                       St_DicInt3         113
672700021104     O          E    11      tCODD4      1
672800021104     O                       staese              85
672900160623     O***       E            tCODd5      1
673000160623     O***                    stavdl              85
673100960906     O          E            TCODD            90
673200960906     O          E            TCODD2      1
673300050413     O               89      IVD(A)        4     73
673400960927     O               89      PERIVA        4     61
673500130117     O              N89      DES(A)              78
673600050413     O                       IMD(A)        4     55
673700130618     O********               VARIED        4     40
673800130618     O********               PRTTPID       4     25
673900960906     O          E            TCODD3      0
674000050413     O                       FATTOTD       4    112
674100050413     O                       FATBOLD       4     97
674200000126     O          E    60      TCODD3      1
674300000124     O                       VIDDIV               6
674400000124     O                                            8 ':'
674500000126     O          E   N60      TCODD3      0
674600000126     O                       VIDDIV               6
674700000126     O                                            8 ':'
674800960906     O          E            RCODD            92
674900960927     O                                          110 'Riporto'
675000960906     O          E            RCODD2      0
675100960927     O                                          113 'SEGUE TOTALI'
675200961104     OQSYSPRT   E            NUMFAT           02
675300961104     O                       RSUT                20
675400060308     O                                           83 'FATTURAZIONE AL'
675500060228     O                       VIDDSP              +3 '  /  /    '
675600070207     O                                          110 'TNSF07R'
675700070207     O                       UDATE              130 '  /  /  '
675800981028     O* lotto di contabilizzazione
675900981028     O          E            NUMFAT           06
676000981028     O                                              'Numero lotto fatturaz.:'
676100981028     O                       knraz             +  2
676200971023     O* FATTURAZIONE MENSILE
676300981028     O          E    20      NUMFAT           08
676400961104     O                                              'FATTURE INIZIO MESE:'
676500961104     O                                         +  2 'PRIMO NUMERO'
676600961104     O                       WINPRI        Z   +  2
676700961104     O                                         +  4 'ULTIMO NUMERO'
676800961104     O                       WINULT        Z   +  2
676900971023     O* FATTURAZIONE NON MENSILE
677000981028     O          E   N20      NUMFAT           08
677100971023     O                                              'FATTURE :'
677200971023     O                                         +  2 'PRIMO NUMERO'
677300971023     O                       WINPRI        Z   +  2
677400971023     O                                         +  4 'ULTIMO NUMERO'
677500971023     O                       WINULT        Z   +  2
677600971023     O* FATTURAZIONE MENSILE
677700971023     O          E    20      NUMFAT           10
677800961104     O                                              'FATTURE FINE   MESE:'
677900961104     O                                         +  2 'PRIMO NUMERO'
678000961104     O                       WFIPRI        Z   +  2
678100961104     O                                         +  4 'ULTIMO NUMERO'
678200961104     O                       WFIULT        Z   +  2
678300000107      * TOTALI FATTURE EMESSE
678400000207     OSYSPRT    E            GEN              02
678500000107     O                       RSUT                20
678600040713     O                       SC(1)               86
678700050413     O                                           99 'TNSF07R'
678800040713     O                       WDTGIO             115 '  /  /    '
678900000107     O                       PAGE          Z    132
679000000107     O                                          127 'PAG.'
679100000107     O          E            GEN              03
679200000107     O                                           48 'A L'
679300040713     O               20      VIDDFI              60 '  /  /    '
679400040713     O              n20      VIDDFT              60 '  /  /    '
679500000107     O          E            GEN              06
679600000107     O                                           15 'TOTALI GENERALI'
679700000107     O          E            GEN              10
679800000107     O                                           17 'Totale imponibile'
679900000125     O                       IMPGEN        2     56
680000000125     O                       VIDDIV              60
680100000125     O               40      IMPGENCNT     2     87
680200000125     O              N42 40                       87 '    '
680300000125     O               40      XSCDIV              92
680400000107     O          E            GEN              11
680500000107     O                                           12 'Totale I.V.A'
680600000125     O                       IVAGEN        2     56
680700000125     O                       VIDDIV              60
680800000125     O               40      IVAGENCNT     2     87
680900000125     O              N42 40                       87 '    '
681000000125     O               40      XSCDIV              92
681100000107     O          E            GEN              13
681200000125     O                                           12 'Totale Bolli'
681300000125     O                       BOLGEN        2     56
681400000125     O                       VIDDIV              60
681500000125     O               40      BOLGENCNT     2     87
681600000125     O              N42 40                       87 '    '
681700000125     O               40      XSCDIV              92
681800000107     O          E            GEN              14
681900000107     O                                           16 'Totale fatturato'
682000000125     O                       TOTGEN        2     56
682100000125     O                       VIDDIV              60
682200000125     O               40      TOTGENCNT     2     87
682300000125     O              N42 40                       87 '    '
682400000125     O               40      XSCDIV              92
682500000107     O          E            GEN              16
682600000107     O                                           14 'Numero fatture'
682700050413     O                       NUMGEN        2     56
682800000107     O          E            GEN              18
682900000125     O                                           15 'TOTALE GENERALE'
683000000125     O                       TOTGEN        2     56
683100000125     O                       VIDDIV              60
683200000125     O                       NUMGEN        2     33
683300000125     O               40      TOTGENCNT     2     87
683400000125     O              N42 40                       87 '    '
683500000125     O               40      XSCDIV              92
683600941107** E
683700060227Manca Anagrafica Cliente
683800060227Codice Pagamento Errato
683900060227Cliente   N O N    Fatturabile
684000050405Errore in fase di contabilizzazione
684100050413Errore Esenzione Iva non presente
684200060227Manca Tariffa
684300060227Fattura con troppe Righe di totale
684400060227Impossibile Fatturare Cliente 8888
684500060227Imponibile           Maggiore del limite fatturabile                 9
684600060227Non Ammesso imponibile=0 per questo Cod.Bolla
684700000107**  SC STAMPA TOTALI
684800040713         - DISTINTA DELLE FATTURE EMESSE - CLIENTI CODIFICATI -
684900060227** cmdc crtdupobj
685000061012CPYF FROMFILE(         /WFMTC00F) TOFILE(QTEMP/WFMTC00F) FROMMBR(WFMTC00F) CRTFILE(*YES)
685100061011CRTDUPOBJ OBJ(WFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
685200061011CLRPFM FILE(QTEMP/WFMTC00F)
685300160426CPYF FROMFILE(         /WFVPD10F) TOFILE(QTEMP/WFVPD10F) FROMMBR(WFVPD10F) CRTFILE(*YES)
685400160426CLRPFM FILE(QTEMP/WFVPD10F)
