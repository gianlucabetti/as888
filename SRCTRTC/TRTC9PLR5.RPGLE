000100141027     H DECEDIT('0,') DATEDIT(*YMD.)
000200141023      *-----------------------------------------------------------------------
000300141027      *  pgm di controllo : si seleziona un  codice LUOGO già inserito
000400141023      *            per evitare di scriverlo diversamente
000500141023      *            OPPURE si deciderà VOLONTARIAMENTE di scriverlo diverso.
000600141023      *-----------------------------------------------------------------------
000700141030      *  DS passaggio parametri
000800141030      * ==========================
000900141030      *
001000141031      *    1----- >OBBL<   PL5LNA_I      3 S  0 LINEA PARTNER  - CAMPO INP
001100141031      *    2--- >dipende<  PL5DAT_I      8 S  0 DATA IN VIGORE - CAMPO INP
001200141031      *           |-->>    PL5LUO_IO    20 A    LUOGO COD.BRT  - CAMPO INP
001300141031      *    3-- or |
001400141031      *           |-->>    PL5PFL_IO    17 A    PLATFORM LIST  - CAMPO I/O
001500141031      *-
001600141031      *                    PL5DESCR_O   35 A    DESC LUOGO/PLST- CAMPO OUT
001700141031      *                    PL5NRREC_O    9 S  0 NR REL.REC.    - CAMPO OUT
001800141031      *                    PL5FLGOK_O    1 A    FLAG RITORNO   - CAMPO OUT
001900141031      *                    PL5DESER_O   50 A    DESC ERRORE    - CAMPO OUT
002000141030      *
002100141030      * Regole:
002200141031      * 1- La linea deve essere passata obbligatoriamente e deve essere EEX
002300141031      * 2- La Data se passata  00000000 imposta UDATE
002400141031      *            se passata  una Data prende il record attivo x quella data
002500141031      *            se passata  99999999 prende il primo record anche se scaduto
002600141031      * 3- Il Luogo o la PList deve essere passata per il controllo.
002700141031      *    Qualora venisse passata la Linea, o la Linea + la data, il Luogo e la PList
002800141031      *    diventano l'OUTPUT che si sta cercando MA SOLO SE vi è un'unico LUOGO/Plist
002900141031      *    sul Partner. Altrimenti verrà segnalato l'errore.
003000141031      *
003100141031      *   Il FLAG RITORNO restituisce "E" -> un eventuale errore
003200141031      *                   restituisce "0" -> se tutto OK
003300141031      *                   restituisce "1" -> no errori ma NON trovato record valido
003400141030      *--------------------------------------------------------------------------------
003500040930      *  11           x selezione di un codice da ripassare al pgm chiamante
003600940307      *  21           GENERICO OPERAZIONI I/O
003700940224      *  22           GENERICO ERRORE OPERAZIONI I/O
003800940224      *  30           SFLDSP
003900940224      * N31           SFLCLR
004000940224      *  31           SFLDSPCTL
004100940224      *  32           SFLNXTCHG
004200940224      *  33           SFLEND
004300940224      *  39           OF PRTF
004400940224      *  40 <---> 49  DSPATR ERRORI SU SFL
004500940608      *  Specificare l'uso dei singoli indicatori
004600940224      *  50 <---> 98  ERRORI SU VIDEO
004700940608      *  Specificare l'uso dei singoli indicatori
004800940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
004900940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
005000940224      *  99           INDIC. GENERALE DI ERRORE
005100940128     F*----------------------------------------------------*
005200141027     FAZORG01L  IF   E           K DISK
005300141027     FEDPFL01L  IF   E           K DISK
005400141017     F                                     INFDS(PFLDS)
005500141017     C*----------------------------------------------------*
005600141017     d PFLds           ds
005700141017     d  PFL_nrr              397    400b 0
005800940207     D*-------------
005900940207     D* Nome PGM a video
006000940207     D                 DS
006100940207     D  PROGR                  1     10
006200940207     D  ASTER1                 1      1    INZ('*')
006300940207     D  SIGLA                  2      9
006400940207     D  ASTER2                10     10    INZ('*')
006500940127     D*-------------
006600141027     D* Passaggio Parametri
006700141031     D KPJBA         E DS
006800141031     d   kpjbus        s                   like (KPJBU)
006900141031     D TRTC9PL5ds    E DS
007000141031     D  tabds        E DS                  extname(TRTC9PLDS)
007100141027     D*-------------
007200141027     D OG143         E DS
007300141027     D*-------------
007400141027     D WLBDA8          DS
007500141027     D  G02DAT                 1      8  0
007600141027     D  G02INV                 9     16  0
007700141027     D  G02ERR                17     17
007800141027     D  G02TGI                18     22  0
007900141027     D*-------------
008000940127     D* Reperimento nome PGM
008100940127     D STATUS         SDS           333
008200940127     D  DSPGM            *PROC
008300090225     D*-------------
008400100119     d dataiso         s               d   datfmt(*iso)
008500100119     d dataeur         s               d   datfmt(*eur)
008600940127     C*----------------------------------------------------*
008700940127     C*                MAIN LINE PROGRAM
008800940127     C*----------------------------------------------------*
008900141027     C* Reperimento parametri
009000141027     C     *ENTRY        PLIST
009100141027     C                   PARM                    KPJBA
009200141027     C*
009300141027     C     KPFL_DATA     KLIST
009400141027     C                   KFLD                    PFLLINEAA
009500141027     C                   KFLD                    PFLLUOGO
009600141027     C                   KFLD                    PFLDTDEC
009700141027     C*
009800141027     C     KPFL_LUOGO    KLIST
009900141027     C                   KFLD                    PFLLINEAA
010000141027     C                   KFLD                    PFLLUOGO
010100141027     C*
010200141027     C                   SETON                                            LR
010300141030     C* se per errore nei campi della DS numerici di INPUT arrivassero dei blank,
010400141030     C* per evitare che si spacchi il programma, reimposto degli (zeri)
010500141027     C                   eval       TRTC9PL5DS = KPJBU
010600141027      *--------
010700141027     c                   clear                   PL5NRREC_O
010800141030     c                   clear                   PL5FLGOK_O
010900141027     c                   clear                   PL5DESER_O
011000141027     c                   clear                   PL5DESCR_O
011100141027      *--------
011200141031     c                   exsr      Linea_arrivo
011300141031      *--------
011400141031     c                   exsr      Data_in_Vigore
011500141027      *--------
011600141031     c                   exsr      Luogo_o_PList
011700141027      *--------
011800141031     c                   exsr      Luogo
011900141031      *--------
012000141031     c                   exsr      PList
012100141027      *--------
012200940325     C* fine programma
012300141030     c                   exsr      FUORI
012400141031      *
012500090225     C************************************************************
012600141031     C* Controlla la LNA passata
012700141030     C************************************************************
012800141031     c     Linea_Arrivo  BEGSR
012900141030      *
013000141031      * Linea di Arrivo
013100141031     c                   iF        PL5LNA_I = 0
013200141031     c                   eval      PL5FLGOK_O = 'E'
013300141031     c                   eval      PL5DESER_O = 'Manca la LINEA di ARRIVO.'
013400141031     c                   exsr      FUORI
013500141031     c                   endiF
013600141031      *--------
013700141031      *  Solo x PARTNER EEX
013800141031     c                   iF        PL5LNA_I > 0
013900141031     c     PL5LNA_I      chain     azorg01l
014000141031     c                   if        %Found(azorg01l)
014100141031     c                   movel     orgde3        og143
014200141031     c                   if        §OGNTW <>'EEX'
014300141031     c                   eval      PL5FLGOK_O = 'E'
014400141031     c                   eval      PL5DESER_O = 'Linea di Arrivo non è EEX'
014500141031     c                   exsr      FUORI
014600141031     c                   end
014700141031     c                   end
014800141031     c                   endiF
014900141030      *
015000141031     c                   ENDSR
015100141031     C************************************************************
015200141031     C* Controlla la LINEA in vigore
015300141031     C************************************************************
015400141031     c     Data_in_VigoreBEGSR
015500141031      *
015600141031      *  Data in Vigore
015700141031      *   se non passata prende l'UDATE quindi nessuna segnalazione di errore
015800141031     c                   iF        PL5DAT_I = 0
015900141031      *   Data A/M/G =(0)  default
016000141031     c************       eval      PL5FLGOK_O = 'E'
016100141031     c***************    eval      PL5DESER_O = 'Manca la Data di riferimento'
016200141031     c************       exsr      FUORI
016300141031      *   Data A/M/G
016400141031     c                   eval      PL5DAT_I =  *date
016500141031     c                   else
016600141031      *--------
016700141031      * controlla la Data
016800141031     c                   iF        PL5DAT_I <> 99999999
016900141031     C                   MOVE      PL5DAT_I      G02INV
017000141031     C                   MOVEL     '3'           G02ERR
017100141031     C                   CALL      'XSRDA8'
017200141031     C                   PARM                    WLBDA8
017300141031     C     G02ERR        IFEQ      '1'
017400141031     c                   eval      PL5FLGOK_O = 'E'
017500141031     c                   eval      PL5DESER_O = 'Data errata o non nel formato -
017600141031     c                             AAAAMMMGG'
017700141031     c                   exsr      FUORI
017800141031     C                   END
017900141031      *
018000141031     c                   end
018100141031     c                   endiF
018200141031      *
018300141031     c                   ENDSR
018400141031     C************************************************************
018500141031     C*  Luogo o Platform  da RESTITUIRE
018600141031     C************************************************************
018700141031     c     LUOGO_o_PLIST BEGSR
018800141031      *
018900141031      *   si può cercare di avere indietro una unica platform list se vi è 1 solo
019000141031      *   luogo.  Allora potrebbe NON servire ricevere il LUOGO o la PlatformLIST.
019100141031      *   Se però, vi sono più luoghi gestiti nello stesso periodo
019200141031      *       allora si deve dare errore.
019300141031     c                   iF        PL5DAT_I <> 99999999
019400141031     c                               and
019500141031     c                             PL5LUO_IO = *blank
019600141031     c                               and
019700141031     c                             PL5PFL_IO = *blank
019800141031     c                   move      'N'           trovato           1
019900141031     c                   z-add     0             conta             3 0
020000141031     C                   eval         PFLLINEAA = PL5LNA_I
020100141031      *
020200141031     C     PFLLINEAA     setll     edPFL01L
020300141031     C     PFLLINEAA     reade     edPFL01L
020400141031     c                   Dow       not %Eof(edPFL01L)
020500141031     C*  range trovato
020600141031     c                   if        PFLDTDEC <= PL5DAT_I
020700141031     c                               and
020800141031     c                             PFLDTSCA >= PL5DAT_I
020900141031     c                   add       1             conta
021000141031      *
021100141031      *   la prima la memorizza comunque
021200141031     c                   if        conta = 1
021300141031      *
021400141031     c                   move      'S'           trovato
021500141031     C                   eval         PL5LUO_IO = PFLLUOGO
021600141031     C                   eval         PL5PFL_IO = PFLCODPFL
021700141031     C                   eval       PL5DESCR_O  = PFLDESCR
021800141031     c                   z-add     PFL_nrr       PL5NRREC_O
021900141031      *
022000141031      *  se ahimé sono di più !!
022100141031     c                   elseif    conta > 1
022200141031      *
022300141031      *  significa che ci sono più LUOGHI e si dovrebbe far scegliere se il pgm
022400141031      *  è richiamato INTERATTIVAMENTE.
022500141031     c                   if        PL5MOD_I = 'I'
022600141031      **
022700141031     c                   exsr      Seleziona
022800141031      **
022900141031      * se l'operatore ha fatto una scelta esce dal ciclo.
023000141031     c                   if        XTANREC > 0
023100141031     c                   move      'S'           trovato
023200141031     c                   LEAVE
023300141031     c                   end
023400141031      *
023500141031     c                   endiF
023600141031      *
023700141031      *   ALTRIMENTI errore ed esce dal ciclo
023800141031      *    pulisce i campi e passa agli step successivi per dare errore
023900141031      *    poichè con solo la linea e la data NON è possibile restituire
024000141031      *    un LUOGO/PLIST fra tanti.
024100141031     c                   move      'N'           trovato
024200141031     C                   clear                   PL5LUO_IO
024300141031     C                   clear                   PL5PFL_IO
024400141031     C                   clear                   PL5DESCR_O
024500141031     c                   clear                   PL5NRREC_O
024600141031     c                   LEAVE
024700141031      *
024800141031     c                   endiF
024900141031      *
025000141031     c                   endiF
025100141031      *
025200141031     C     PFLLINEAA     reade     edPFL01L
025300141031     c                   endDo
025400141031      *
025500141031      *  se ha trovato un solo record allora può restituire correttamente
025600141031      *   l'informazione al chiamante
025700141031     c                   if        trovato = 'S'
025800141031     c                   eval      PL5FLGOK_O = '0'
025900141031     c                   exsr      FUORI
026000141031     c                   endiF
026100141031      *
026200141031     c                   endiF
026300141031      *--------
026400141031      * deve ricevere il LUOGO o la PLATFORMLIST
026500141031     c                   iF        PL5LUO_IO = *blank
026600141031     c                               and
026700141031     c                             PL5PFL_IO = *blank
026800141031     c                   eval      PL5FLGOK_O = 'E'
026900141031     c                   eval      PL5DESER_O = 'Inserire LUOGO o PLIST per la -
027000141031     c                             LINEA.'
027100141031     c                   exsr      FUORI
027200141031     c                   endiF
027300141031      *
027400141031      *
027500141031     c                   ENDSR
027600141031     C************************************************************
027700141031     C* Seleziona fra enne il Luogo per la LINEA PArtner
027800141031     C************************************************************
027900141031     c     Seleziona     BEGSR
028000141031      *
028100141031     C                   RESET                   tabds
028200141031     c                   move      '01'          xtaopz
028300141031     C                   MOVE      *ZERO         xtaret
028400141031     C                   MOVE      *ZERO         xtaopr
028500141031     C                   MOVEl     PL5LNA_I      XTAKLNA
028600141031     C                   MOVEl     *blank        XTAKLUO
028700141031     C                   MOVEl     PL5DAT_I      XTAKDAT
028800141031     C                   MOVEl     *zeros        XTANREC
028900141031      *
029000141031     C                   MOVEL     KPJBU         KPJBUS
029100141031     C                   MOVEL(p)  tabds         KPJBU
029200141031$004 C                   CALL      'TRTC9PLR1'
029300141031     C                   PARM                    KPJBA
029400141031     C                   MOVEL     KPJBU         tabds
029500141031     C                   MOVEL     KPJBUS        KPJBU
029600141031      * imposta il LUOGO
029700141031     C                   eval      PL5LUO_IO = XTAKLUO
029800141031      *
029900141031     c                   ENDSR
030000141031     C************************************************************
030100141031     C* Controlla il LUOGO passato
030200141031     C************************************************************
030300141031     c     LUOGO         BEGSR
030400141031      *
030500141031      *-*
030600141031      *  Se ricevuto il LUOGO  - si vuole restituita la PlatformList
030700141031      *-*
030800141031     c                   iF        PL5LUO_IO <> *blank
030900141031     c                   move      'N'           trovato           1
031000141031     C                   eval         PFLLINEAA = PL5LNA_I
031100141031     C                   eval         PFLLUOGO  = PL5LUO_IO
031200141031     C                   eval         PFLDTDEC  = PL5DAT_I
031300141031     C     KPFL_DATA     setgt     edPFL01L
031400141031     C     KPFL_LUOGO    readpe    edPFL01L
031500141031     c                   Dow       not %Eof(edPFL01L)
031600141031     C*  range trovato
031700141031     c                   if        PFLDTDEC <= PL5DAT_I
031800141031     c                               and
031900141031     c                             PFLDTSCA >= PL5DAT_I
032000141031     c                               or
032100141031     c                             PL5DAT_I = 99999999
032200141031      *
032300141031     C                   eval         PL5PFL_IO = PFLCODPFL
032400141031     C                   eval       PL5DESCR_O  = PFLDESCR
032500141031     c                   z-add     PFL_nrr       PL5NRREC_O
032600141031     c                   eval      PL5FLGOK_O = '0'
032700141031     c                   move      'S'           trovato
032800141031     c                   exsr      FUORI
032900141031     c                   end
033000141031     C*
033100141031     C     KPFL_LUOGO    readpe    edPFL01L
033200141031     c                   endDo
033300141031      *
033400141031      *  con i parametri passati NON è stato trovato nulla
033500141031     c                   if        Trovato = 'N'
033600141031     c                   eval      PL5FLGOK_O = '1'
033700141031     c                   eval      PL5DESER_O = 'Non trovato cod.Platform LIST.'
033800141031     c                   exsr      FUORI
033900141031     c                   end
034000141031      *
034100141031     c                   endiF
034200141031      *
034300141031      *
034400141031     c                   ENDSR
034500141031     C************************************************************
034600141031     C* Controlla la PLIST passata
034700141031     C************************************************************
034800141031     c     PLIST         BEGSR
034900141031      *
035000141031      *-*
035100141031      *  Se ricevuto la PlatformLIST  - si vuole verificare l'esistenza
035200141031      *-*
035300141031     c                   iF        PL5PFL_IO <> *blank
035400141031     c                   move      'N'           trovato           1
035500141031     C                   eval         PFLLINEAA = PL5LNA_I
035600141031     C     PFLLINEAA     setll     edPFL01L
035700141031     C     PFLLINEAA     reade     edPFL01L
035800141031     c                   Dow       not %Eof(edPFL01L)
035900141031     C*  Plist trovato
036000141031     C                   If        PL5PFL_IO = PFLCODPFL
036100141031     C*   nel range di date
036200141031     c                   if        PFLDTDEC <= PL5DAT_I
036300141031     c                               and
036400141031     c                             PFLDTSCA >= PL5DAT_I
036500141031     c                               or
036600141031     c                             PL5DAT_I = 99999999
036700141031      *
036800141031     C                   eval         PL5LUO_IO = PFLLUOGO
036900141031     C                   eval       PL5DESCR_O  = PFLDESCR
037000141031     c                   z-add     PFL_nrr       PL5NRREC_O
037100141031     c                   eval      PL5FLGOK_O = '0'
037200141031     c                   move      'S'           trovato
037300141031     c                   exsr      FUORI
037400141031     c                   end
037500141031     c                   end
037600141031     C*
037700141031     C     PFLLINEAA     reade     edPFL01L
037800141031     c                   endDo
037900141031      *
038000141031      *  con i parametri passati NON è stato trovato nulla
038100141031     c                   if        Trovato = 'N'
038200141031     c                   eval      PL5FLGOK_O = '1'
038300141031     c                   eval      PL5DESER_O = 'Cod.Platform LIST non trovata'
038400141031     c                   exsr      FUORI
038500141031     c                   end
038600141031      *
038700141031     c                   endiF
038800141031      *
038900141031     c                   ENDSR
039000141031     C************************************************************
039100141031     C* Esce dal programma
039200141031     C************************************************************
039300141031     c     FUORI         BEGSR
039400141031      *
039500141031     c                   eval      kpjbu = TRTC9PL5DS
039600141031     c                   RETURN
039700141031      *
039800141031     c                   endsr
039900141030     C************************************************************
