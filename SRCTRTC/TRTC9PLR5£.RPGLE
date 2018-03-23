000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200141027     H DECEDIT('0,') DATEDIT(*YMD.)
000300141023      *-----------------------------------------------------------------------
000400141027      *  pgm di controllo : si seleziona un  codice LUOGO già inserito
000500141023      *            per evitare di scriverlo diversamente
000600141023      *            OPPURE si deciderà VOLONTARIAMENTE di scriverlo diverso.
000700141023      *-----------------------------------------------------------------------
000800141030      *  DS passaggio parametri
000900141030      * ==========================
001000141030      *
001100141031      *    1----- >OBBL<   PL5LNA_I      3 S  0 LINEA PARTNER  - CAMPO INP
001200141031      *    2--- >dipende<  PL5DAT_I      8 S  0 DATA IN VIGORE - CAMPO INP
001300141031      *           |-->>    PL5LUO_IO    20 A    LUOGO COD.BRT  - CAMPO INP
001400141031      *    3-- or |
001500141031      *           |-->>    PL5PFL_IO    17 A    PLATFORM LIST  - CAMPO I/O
001600141031      *-
001700141031      *                    PL5DESCR_O   35 A    DESC LUOGO/PLST- CAMPO OUT
001800141031      *                    PL5NRREC_O    9 S  0 NR REL.REC.    - CAMPO OUT
001900141031      *                    PL5FLGOK_O    1 A    FLAG RITORNO   - CAMPO OUT
002000141031      *                    PL5DESER_O   50 A    DESC ERRORE    - CAMPO OUT
002100141030      *
002200141030      * Regole:
002300141031      * 1- La linea deve essere passata obbligatoriamente e deve essere EEX
002400141031      * 2- La Data se passata  00000000 imposta UDATE
002500141031      *            se passata  una Data prende il record attivo x quella data
002600141031      *            se passata  99999999 prende il primo record anche se scaduto
002700141031      * 3- Il Luogo o la PList deve essere passata per il controllo.
002800141031      *    Qualora venisse passata la Linea, o la Linea + la data, il Luogo e la PList
002900141031      *    diventano l'OUTPUT che si sta cercando MA SOLO SE vi è un'unico LUOGO/Plist
003000141031      *    sul Partner. Altrimenti verrà segnalato l'errore.
003100141031      *
003200141031      *   Il FLAG RITORNO restituisce "E" -> un eventuale errore
003300141031      *                   restituisce "0" -> se tutto OK
003400141031      *                   restituisce "1" -> no errori ma NON trovato record valido
003500141030      *--------------------------------------------------------------------------------
003600040930      *  11           x selezione di un codice da ripassare al pgm chiamante
003700940307      *  21           GENERICO OPERAZIONI I/O
003800940224      *  22           GENERICO ERRORE OPERAZIONI I/O
003900940224      *  30           SFLDSP
004000940224      * N31           SFLCLR
004100940224      *  31           SFLDSPCTL
004200940224      *  32           SFLNXTCHG
004300940224      *  33           SFLEND
004400940224      *  39           OF PRTF
004500940224      *  40 <---> 49  DSPATR ERRORI SU SFL
004600940608      *  Specificare l'uso dei singoli indicatori
004700940224      *  50 <---> 98  ERRORI SU VIDEO
004800940608      *  Specificare l'uso dei singoli indicatori
004900940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
005000940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
005100940224      *  99           INDIC. GENERALE DI ERRORE
005200940128     F*----------------------------------------------------*
005300141027     FAZORG01L  IF   E           K DISK
005400141027     FEDPFL01L  IF   E           K DISK
005500141017     F                                     INFDS(PFLDS)
005600141017     C*----------------------------------------------------*
005700141017     d PFLds           ds
005800141017     d  PFL_nrr              397    400b 0
005900940207     D*-------------
006000940207     D* Nome PGM a video
006100940207     D                 DS
006200940207     D  PROGR                  1     10
006300940207     D  ASTER1                 1      1    INZ('*')
006400940207     D  SIGLA                  2      9
006500940207     D  ASTER2                10     10    INZ('*')
006600940127     D*-------------
006700141027     D* Passaggio Parametri
006800141027     D KPJBA         E DS
006900141027     D TRTC9PL5DS    E DS
007000141027     D*-------------
007100141027     D OG143         E DS
007200141027     D*-------------
007300141027     D WLBDA8          DS
007400141027     D  G02DAT                 1      8  0
007500141027     D  G02INV                 9     16  0
007600141027     D  G02ERR                17     17
007700141027     D  G02TGI                18     22  0
007800141027     D*-------------
007900940127     D* Reperimento nome PGM
008000940127     D STATUS         SDS           333
008100940127     D  DSPGM            *PROC
008200090225     D*-------------
008300100119     d dataiso         s               d   datfmt(*iso)
008400100119     d dataeur         s               d   datfmt(*eur)
008500141023     D*-------------
008600141023     D LUOGO           s             20    DIM(100)
008700141023     D wrkLUOGO        s             20
008800940207     D*-------------
008900940211     D* COSTANTI
009000141027     D*-------------
009100940127     C*----------------------------------------------------*
009200940127     C*                MAIN LINE PROGRAM
009300940127     C*----------------------------------------------------*
009400141027     C* Reperimento parametri
009500141027     C     *ENTRY        PLIST
009600141027     C                   PARM                    KPJBA
009700141027     C*
009800141027     C     KPFL_DATA     KLIST
009900141027     C                   KFLD                    PFLLINEAA
010000141027     C                   KFLD                    PFLLUOGO
010100141027     C                   KFLD                    PFLDTDEC
010200141027     C*
010300141027     C     KPFL_LUOGO    KLIST
010400141027     C                   KFLD                    PFLLINEAA
010500141027     C                   KFLD                    PFLLUOGO
010600141027     C*
010700141027     C                   SETON                                            LR
010800141030     C* se per errore nei campi della DS numerici di INPUT arrivassero dei blank,
010900141030     C* per evitare che si spacchi il programma, reimposto degli (zeri)
011000141027     C                   eval       TRTC9PL5DS = KPJBU
011100141027      *--------
011200141027     c                   clear                   PL5NRREC_O
011300141030     c                   clear                   PL5FLGOK_O
011400141027     c                   clear                   PL5DESER_O
011500141027     c                   clear                   PL5DESCR_O
011600141027      *--------
011700141027      * Linea di Arrivo
011800141027     c                   iF        PL5LNA_I = 0
011900141030     c                   eval      PL5FLGOK_O = 'E'
012000141030     c                   eval      PL5DESER_O = 'Manca la LINEA di ARRIVO.'
012100141030     c                   exsr      FUORI
012200141027     c                   endiF
012300141027      *--------
012400141027      *  Solo x PARTNER EEX
012500141027     c                   iF        PL5LNA_I > 0
012600141027     c     PL5LNA_I      chain     azorg01l
012700141027     c                   if        %Found(azorg01l)
012800141027     c                   movel     orgde3        og143
012900141027     c                   if        §OGNTW <>'EEX'
013000141030     c                   eval      PL5FLGOK_O = 'E'
013100141030     c                   eval      PL5DESER_O = 'Linea di Arrivo non è EEX'
013200141030     c                   exsr      FUORI
013300141027     c                   end
013400141027     c                   end
013500141027     c                   endiF
013600141027      *--------
013700141027      *  Data in Vigore
013800141027      *   se non passata prende l'UDATE quindi nessuna segnalazione di errore
013900141027     c                   iF        PL5DAT_I = 0
014000141031      *   Data A/M/G =(0)  default
014100141030     c************       eval      PL5FLGOK_O = 'E'
014200141030     c***************    eval      PL5DESER_O = 'Manca la Data di riferimento'
014300141030     c************       exsr      FUORI
014400141027      *   Data A/M/G
014500141027     c                   eval      PL5DAT_I =  *date
014600141027     c                   else
014700141027      *--------
014800141027      * controlla la Data
014900141030     c                   iF        PL5DAT_I <> 99999999
015000141027     C                   MOVE      PL5DAT_I      G02INV
015100141027     C                   MOVEL     '3'           G02ERR
015200141027     C                   CALL      'XSRDA8'
015300141027     C                   PARM                    WLBDA8
015400141027     C     G02ERR        IFEQ      '1'
015500141030     c                   eval      PL5FLGOK_O = 'E'
015600141030     c                   eval      PL5DESER_O = 'Data errata o non nel formato -
015700141030     c                             AAAAMMMGG'
015800141030     c                   exsr      FUORI
015900141027     C                   END
016000141027      *
016100141030     c                   end
016200141027     c                   endiF
016300141027      *--------
016400141031      *   si potrebbe cercare di avere indietro una unica platform list se vi è 1 solo
016500141031      *   luogo.  Allora potrebbe NON servire ricevere il LUOGO o la PlatformLIST.
016600141031      *   Se però, vi sono più luoghi gestiti nello stesso periodo allora si deve dare errore.
016700141031     c                   iF        PL5DAT_I <> 99999999
016800141031     c                               and
016900141031     c                             PL5LUO_IO = *blank
017000141031     c                               and
017100141031     c                             PL5PFL_IO = *blank
017200141031     c                   move      'N'           trovato           1
017300141031     c                   z-add     0             conta             3 0
017400141031     C                   eval         PFLLINEAA = PL5LNA_I
017500141031     C     PFLLINEAA     setll     edPFL01L
017600141031     C     PFLLINEAA     reade     edPFL01L
017700141031     c                   Dow       not %Eof(edPFL01L)
017800141031     C*  range trovato
017900141031     c                   if        PFLDTDEC <= PL5DAT_I
018000141031     c                               and
018100141031     c                             PFLDTSCA >= PL5DAT_I
018200141031     c                   add       1             conta
018300141031      *   la prima la memorizza
018400141031     c                   if        conta = 1
018500141031     C                   eval         PL5LUO_IO = PFLLUOGO
018600141031     C                   eval         PL5PFL_IO = PFLCODPFL
018700141031     C                   eval       PL5DESCR_O  = PFLDESCR
018800141031     c                   z-add     PFL_nrr       PL5NRREC_O
018900141031      *
019000141031     c                   else
019100141031      *   se c'è già una seconda alternativa, allora  pulisce i campi
019200141031      *    e passa agli step successivi per segnalare l'errore poichè
019300141031      *    con solo la linea e la data NON è in grado di restituire un
019400141031      *    solo LUOGO/PLIST.
019500141031     C                   clear                   PL5LUO_IO
019600141031     C                   clear                   PL5PFL_IO
019700141031     C                   clear                   PL5DESCR_O
019800141031     c                   clear                   PL5NRREC_O
019900141031     c                   leave
020000141031     c                   endiF
020100141031      *
020200141031     c                   endiF
020300141031      *
020400141031     C     PFLLINEAA     reade     edPFL01L
020500141031     c                   endDo
020600141031      *
020700141031      *  se ha trovato un solo record allora può restituire correttamente
020800141031      *   l'informazione al chiamante
020900141031     c                   if        conta = 1
021000141031     c                   eval      PL5FLGOK_O = '0'
021100141031     c                   exsr      FUORI
021200141031     c                   endiF
021300141031      *
021400141031     c                   endiF
021500141031      *--------
021600141027      * deve ricevere il LUOGO o la PLATFORMLIST
021700141027     c                   iF        PL5LUO_IO = *blank
021800141027     c                               and
021900141027     c                             PL5PFL_IO = *blank
022000141030     c                   eval      PL5FLGOK_O = 'E'
022100141030     c                   eval      PL5DESER_O = 'Manca il LUOGO o il cod.PLIST -
022200141030     c                             da verificare.'
022300141030     c                   exsr      FUORI
022400141027     c                   endiF
022500141027      *
022600141027      *-*************************************************************************-
022700141027      *  Se ricevuto il LUOGO  - si vuole restituita la PlatformList
022800141027      *-*************************************************************************-
022900141027     c                   iF        PL5LUO_IO <> *blank
023000141027     c                   move      'N'           trovato           1
023100141027     C                   eval         PFLLINEAA = PL5LNA_I
023200141027     C                   eval         PFLLUOGO  = PL5LUO_IO
023300141027     C                   eval         PFLDTDEC  = PL5DAT_I
023400141027     C     KPFL_DATA     setgt     edPFL01L
023500141027     C     KPFL_LUOGO    readpe    edPFL01L
023600141027     c                   Dow       not %Eof(edPFL01L)
023700141027     C*  range trovato
023800141030     c                   if        PFLDTDEC <= PL5DAT_I
023900141027     c                               and
024000141030     c                             PFLDTSCA >= PL5DAT_I
024100141030     c                               or
024200141030     c                             PL5DAT_I = 99999999
024300141030      *
024400141027     C                   eval         PL5PFL_IO = PFLCODPFL
024500141027     C                   eval       PL5DESCR_O  = PFLDESCR
024600141027     c                   z-add     PFL_nrr       PL5NRREC_O
024700141030     c                   eval      PL5FLGOK_O = '0'
024800141027     c                   move      'S'           trovato
024900141030     c                   exsr      FUORI
025000141027     c                   end
025100141027     C*
025200141027     C     KPFL_LUOGO    readpe    edPFL01L
025300141027     c                   endDo
025400141027      *
025500141027      *  con i parametri passati NON è stato trovato nulla
025600141027     c                   if        Trovato = 'N'
025700141030     c                   eval      PL5FLGOK_O = '1'
025800141030     c                   eval      PL5DESER_O = 'Non trovato cod.Platform LIST.'
025900141030     c                   exsr      FUORI
026000141027     c                   end
026100141027      *
026200141027     c                   endiF
026300141027      *
026400141027      *-*************************************************************************-
026500141027      *  Se ricevuto la PlatformLIST  - si vuole verificare l'esistenza
026600141027      *-*************************************************************************-
026700141027     c                   iF        PL5PFL_IO <> *blank
026800141027     c                   move      'N'           trovato           1
026900141027     C                   eval         PFLLINEAA = PL5LNA_I
027000141027     C     PFLLINEAA     setll     edPFL01L
027100141027     C     PFLLINEAA     reade     edPFL01L
027200141027     c                   Dow       not %Eof(edPFL01L)
027300141027     C*  Plist trovato
027400141030     C                   If        PL5PFL_IO = PFLCODPFL
027500141030     C*   nel range di date
027600141030     c                   if        PFLDTDEC <= PL5DAT_I
027700141030     c                               and
027800141030     c                             PFLDTSCA >= PL5DAT_I
027900141030     c                               or
028000141030     c                             PL5DAT_I = 99999999
028100141030      *
028200141027     C                   eval         PL5LUO_IO = PFLLUOGO
028300141027     C                   eval       PL5DESCR_O  = PFLDESCR
028400141027     c                   z-add     PFL_nrr       PL5NRREC_O
028500141030     c                   eval      PL5FLGOK_O = '0'
028600141027     c                   move      'S'           trovato
028700141030     c                   exsr      FUORI
028800141027     c                   end
028900141030     c                   end
029000141027     C*
029100141027     C     PFLLINEAA     reade     edPFL01L
029200141027     c                   endDo
029300141027      *
029400141027      *  con i parametri passati NON è stato trovato nulla
029500141027     c                   if        Trovato = 'N'
029600141030     c                   eval      PL5FLGOK_O = '1'
029700141030     c                   eval      PL5DESER_O = 'Cod.Platform LIST non trovata'
029800141030     c                   exsr      FUORI
029900141027     c                   end
030000141027      *
030100141027     c                   endiF
030200141027      *--------
030300141027      *
030400141027     c
030500940325     C* fine programma
030600141030     c                   exsr      FUORI
030700090225     C************************************************************
030800141030     C* Esce dal programma
030900141030     C************************************************************
031000141030     c     FUORI         begsr
031100141030      *
031200141030     c                   eval      kpjbu = TRTC9PL5DS
031300141030     c                   RETURN
031400141030      *
031500141030     c                   endsr
031600141030     C************************************************************
