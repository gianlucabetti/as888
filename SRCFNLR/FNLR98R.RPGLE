000100110124     h dftactgrp(*no) actgrp(*caller)
000200000810     H DECEDIT('0,') DATEDIT(*DMY.)
000300030730      * FNLR98R *----------------------------------------------------*
000400930428      *---------*                                                    *
000500101220      *     - TOGLI E METTI IN DISTINTA x SPEDIZIONE -               *
000600101221      *--------------------------------------------------------------*
000700101222      *     -    per ora il METTI è solo in occulto  -               *
000800101220      *     -    bisognerebbe aggiungere a video il  -               *
000900101220      *     -    campo Numero Distinta su cui inserire.              *
001000101221      *--------------------------------------------------------------*
001100101213      * ================                                             *
001200101213      * GESTIONE COMMIT                                              *
001300101213      * ================                                             *
001400101222      *  Deve essere fatta direttamente qua dentro poichè il progr.  *
001500101222      *  è gestito adesso anche in cascata con altri programmi che   *
001600101213      *  potrebbero non gestire il commit.                           *
001700101213      *  Per correttezza e sicurezza gli aggiornamenti devono essere *
001800101213      *  a livello di spedizione.                                    *
001900101213      *  - Attenzione quando non si deve fare il COMMIT -            *
002000101220     F*--------------------------------------------------------------*
002100101220      ***Se richiamato la DS ha i seguenti campi:                                     ***
002200101220      ***   I98AAS         1     4 S  0 ANNO SPEDIZ. ORIG.                            ***
002300101220      ***   I98LNP         5     3 S  0 LNP SPEDIZ. ORIG.                             ***
002400101220      ***   I98NRS         8     2 S  0 SERIE SPEDIZ. ORIG.                           ***
002500101220      ***   I98NSP        10     7 S  0 NR. SPEDIZ. ORIG.                             ***
002600101220      ***   O98ESITO      17     1 A    ESITO 0 = OK 1 = ERRORE                       ***
002700101220      ***   I98DELINS     18     1 A    BLK=TOGLI/M=METTI                             ***
002800101220      ***   I98INTERR     19     1 A    BLK=NO/S=SI INTERROGAZIONE                    ***
002900101220      ***   I98STRCMT     20     1 A    BLK=SI/N=NO X FARE STRCMTCTL                  ***
003000101220      ***   I98VIDEO      21     1 A    BLK=SI/N=NO GESTIONE VIDEO                    ***
003100101220      ***   I98ALCDST     22     1 A    BLK=SI/N=NO CTL.SE DIST.ALLOCATA              ***
003200101220      ***   I98CTLDST     23     1 A    BLK=SI/N=NO CONTROLLARE LA DISTINTA           ***
003300110103      ***   I98FGS        24     3 S  0 FILIALE GES.DIST.OBBLIGATORIOSE RICHIAMATO    ***
003400110103      ***   I98NDC        27     6 S  0 NR.DIST.OBBLIGATORIO SE RICHIAMATO            ***
003500101220      ***   O98MSGERR     35    66 A    MSG DI ERRORE ESPLICITO                       ***
003600101213     F*--------------------------------------------------------------*
003700160607     FFNLR98D   CF   E             WORKSTN usropn
003800101223     F*--------------------------------------------------------------*
003900021204     Ffiapd01L  IF   E           K DISK
004000101214     Ftntbe01L  IF   E           K DISK
004100101223      *----
004200110125     FFNARB01L  UF   E           K DISK    commit(on) infds(arbds) usropn
004300110125     FFiarg01L  UF a E           K DISK    commit infds(argds) usropn
004400030730     FTABEL00F  iF   E           K DISK
004500030730     Ffnanm01l  iF   E           K DISK
004600090605     Ffnlbl01l  iF   E           K DISK
004700110125     FFNEVB04L  UF A E           K DISK    commit usropn
004800070316     Fazorg01l  IF   E           K DISK
004900101223      *----
005000101223     Ffidst09L  IF   E           K DISK
005100940321      *---------------------------------------------------------------
005200030730     d* schiere
005300030730      *---------------------------------------------------------------
005400160316     D ERR             S             66    DIM(21) CTDATA PERRCD(1)             MSG ERRORE
005500101213      *---------------------------------------------------------------
005600030730     d* ds esterne
005700030730      *---------------------------------------------------------------
005800030730     D KPJBA         E DS
005900030730     D ddatiute      e ds
006000030730     D azuteds       e ds                  extname(AZUTE00F)
006100160509     D fidstds       e ds                  extname(FIDST00F) inz
006200110105     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
006300110105     d fnarbds       e ds                  extname(fnarb00F)
006400110107     d fiargds$      e ds                  extname(fiarg00F) prefix($)
006500110107     d fiargds       e ds                  extname(fiarg00F)
006600030730     D tibs34ds      E DS                  inz
006700101221     D* DS PER - Bolle valide in consegna
006800070517     D fidg37ds      E DS
006900101220      * msg allocazione  inviato all'utente
007000101216     D trul82ds      E DS
007100080416     D ddstflr       E DS
007200100202     D dargflo       E DS
007300080416     c* elimina dati a pda
007400080416     D fidg42ds      E DS
007500080416     D fidg44ds      E DS
007600110105     D* DS PER - aggiornamento di ARB
007700110105     D fidg23ds      E DS
007701160316     D Dnsd          E DS
007800101214     D DDGD          E DS
007900100514     D DS7r          E DS
008000070316     D og146         E DS                  inz
008100080416     D og148         E DS                  inz
008200101222      * DS per richiamo pgm fnlr99r x gestione ORM in particolarità RC
008300030826     D fnlr99ds      E DS
008400031125      * DS per richiamo da fnlr50r x togliere bolla da distinta
008500031125     D fnlr98ds      E DS
008600141112     D fnlr98ds2     E DS
008700101223      * DS per controllo e allocazione Distinta
008800101223     D fnlrq3ds      E DS
008900101223      * DS per controllo disponibilità Bolla da assegnare a una distinta
009000101223     D fnlrq4ds      E DS
009100030701      * DS per impostare default se con pistola o manuale
009200080605     D combts          s              1
009300030730     d* anomalie
009400030730     D DSLR33        E DS                  EXTNAME(FNLR33DS)
009500030730     c*po gestione
009600030730     d trul06ds      e ds
009700030730     d  lint                   1     90  0 dim(30)
009800030730     c*controllo check digits
009900030730     D trul28ds      E DS
010000101214      *
010100101214     D WLBDA8          DS
010200101214     D  G02DAT                 1      8  0
010300101214     D  G02INV                 9     16  0
010400101214     D  G02ERR                17     17
010500101214     D  G02TGI                18     22  0
010600101214      *
010700101214     d Wdata8          DS
010800101214     d  dadata                 1      8  0
010900101214     d  adata                  9     16  0
011000101214     d  GioLav                17     21  0
011100101214      *
011200101214     D DATPAR          DS
011300101214     D  GIODAT                 1      8  0
011400101214     D  GIOINV                 9     16  0
011500101214     D  GIOTGI                17     21  0
011600030730      *---------------------------------------------------------------
011700030730     d* ds interne
011800030730      *---------------------------------------------------------------
011900941107      * DS per richiamo pgm FNLR36R
012000941108     D DSLR36          DS
012100941108     D  P36AAS                14     17  0
012200941108     D  P36LNP                18     20  0
012300941108     D  P36NRS                21     22  0
012400941108     D  P36NSP                23     29  0
012500941108     D  P36F03                30     30
012600941108     D  P36FLG                31     31
012700131104     c*
012800131104     d dEVB01        e ds
012900131104     c*
013000030730     d* num.relativo di rec. x file anomalie
013100990519     D FNANM1          DS
013200960415     D  ANMNR1               397    400B 0
013300030618     c* ds per sottocampare il barcode della spedizione
013400030618     D ds_pis          DS
013500030618     D  ds_pisaas                     2  0
013600030618     D  ds_pislnp                     3  0
013700030618     D  ds_pisnrs                     2  0
013800030618     D  ds_pisnsp                     7  0
013900030623     D  ds_chkdgt                     1  0
014000030730     d*---------------------------------------------------------------*
014100030730     d* campi di comodo                                               *
014200030730     d*---------------------------------------------------------------*
014300030730     d savchk          s                   like(o28ckd)
014400111020     d savpis          s                   like(v03pis)
014500111020     d savans          s                   like(v03ans)
014600110804     d savlnp          s                   like(v03lnp)
014700110804     d savnrs          s                   like(v03nrs)
014800110804     d savnsp          s                   like(v03nsp)
014900030730     d barcode         s             14
015000030730     d dtaeur          s               d   datfmt(*eur)
015100030730     d dtaiso          s               d
015200101214     d dtaiso_oggi     s               d   datfmt(*iso)
015300101214     d dtaiso_dst      s               d   datfmt(*iso)
015400110804     D okprfo          S              1    INZ(' ')
015500101213      *-----
015600101213     D Found_Dist      S              1    INZ('N')
015700101213     D Error_Dist      S              1    INZ('N')
015800101213     D Rec_allocato    S              1    INZ('N')
015900101222     D Forza_aggiornamento...
016000101222     D                 s              1    INZ('N')
016100101223     D ALLOCA_Distinta...
016200101223     D                 s              1
016300110104     D ARG_Allocato...
016400110104     D                 s              1
016500101213     D*-------------
016600101213     d dstds           ds
016700101213     d  dst_nrr              397    400b 0
016800101217      *
016900101217     d argds           ds
017000101217     d  arg_nrr              397    400b 0
017100101217      *
017200101217     d arbds           ds
017300101217     d  arb_nrr              397    400b 0
017400101217      *
017500101222     C*-----------------------------------------------------*
017600101213      *
017700101214     D digits          c                   '0123456789'
017800101217     D Mettere         c                   'M'
017900101217     D Togliere        c                   ' '
018000101220     D SI              c                   'S'
018100101220     D NO              c                   'N'
018200110125     D on              s              1
018300911016      *---------------------------------------------------------------*
018400911016      * FLUSSO PRINCIPALE                                             *
018500911016      *---------------------------------------------------------------*
018600110104     C*
018700110104     C*   subito accende per usufruire del RETURN
018800110104     c                   seton                                        LR
018900110104     C*
019000110104     C* Se RICHIAMATO
019100110104     c                   if        Richiamato = SI
019200110104      * imposta i campi del video come se li avessero digitati
019300110104     c                   z-add     i98aas        v03ans
019400110104     c                   z-add     i98lnp        v03lnp
019500110104     c                   z-add     i98nsp        v03nsp
019600110104     c                   z-add     i98nrs        v03nrs
019700110104     c                   setoff                                       73
019800110104      **
019900110104      **  parametri obbligatori  FILIALE di GESTIONE
020000110104     c                   if        i98fgs = 0
020100110104     c                   eval      O98msgERR = ERR(14)
020200110324     c                   GOTO      FINE
020300110324     c************       return
020400110104     c                   end
020500110104      *
020600110104      **  parametri obbligatori  DISTINTA SU CUI ESEGUIRE togli o metti
020700110104     c                   if        i98ndc = 0
020800110104     c                   eval      O98msgERR = ERR(15)
020900110324     c                   GOTO      FINE
021000110324     c************       return
021100110104     c                   end
021200110104     c                   end
021300110104     C*
021400030730     C                   DO        *HIVAL
021500101222      * -----------
021600101216     C* VIDEO
021700101220      *--------  BYPASSATO se richiamato in OCCULTO
021800101220      **              --------------------------
021900101221     c                   if        Visual_Display = SI
022000101217      *--------
022100030730     c* accendo o spengo l'indicatore 73 che mi serve per il posizionamento
022200030730     c* 73 in caso di errore si posiziona sul barcode altrimenti sul numero
022300030730     c* della spedizione
022400030730     c                   if        *in90
022500030730     c                   if        (v03pis <> ' ' and v03nsp <> 0) or
022600101213     c                             (v03pis  = ' ' and v03nsp  = 0)
022700080605     c     combts        comp      'L'                                    73
022800030730     c                   end
022900101213     c                   if        (v03pis <> ' ' and v03nsp  = 0) or
023000101213     c                             (v03pis  = ' ' and v03nsp <> 0)
023100030730     c     v03pis        comp      ' '                                7373
023200030730     c                   end
023300101213      *
023400030730     c                   setoff                                       90
023500030730     C                   write     LR98D01
023600030730     c                   seton                                        90
023700030730     c                   end
023800030730     c*
023900101220     c                   If        Richiamato = SI
024000101222      * imposta i campi del video come se li avessero digitati
024100101222     c                   z-add     i98aas        v03ans
024200101222     c                   z-add     i98lnp        v03lnp
024300101222     c                   z-add     i98nsp        v03nsp
024400101222     c                   z-add     i98nrs        v03nrs
024500031125     c                   setoff                                       73
024600031125     c                   endif
024700101213     c*
024800101213      *                *-------------------*
024900030730     C                   EXFMT     LR98D01
025000101213      *                *-------------------*
025100110804     C*  Imposto variabili x test forzature
025200111020     c                   if        combts =  'L'
025300111020     C     V03pis        ifNE      SAVpis
025400111020     c                   clear                   okprfo
025500111020     C                   END
025600111020     c                   else
025700111020     C     v03ANS        IFNE      SAVANS
025800111020     C     V03LNP        ORNE      SAVLNP
025900111020     C     V03NSP        ORNE      SAVNSP
026000111020     C     V03NRS        ORNE      SAVNRS
026100111020     c                   clear                   okprfo
026200111020     C                   END
026300111020     c                   end
026400030730     c* fine lavoro
026500941107     C     *INKC         IFEQ      '1'
026600030730     C                   LEAVE
026700941107     C                   END
026800110808     c* forzatura per perfect order
026900110808     c                   if        *inkh
027000110808     c                   eval      okprfo = *on
027100110808     c                   end
027200101213      *--------
027300101220      ** IN OCCULTO SONO GIA' IMPOSTATI I CAMPI DELLA SPEDIZIONE
027400101213     c                   endif
027500101220      *--------      --------------------------------
027600030730     c* controlli
027700101220     c                   exsr      srctr
027800101223      *
027900141112     c                   if        visual_display = si
028000141114     c                             and forzato = *blank
028100141117     c                             and not *in90
028200141112     c*check spedizione con disposizioni di dirottamento richiede forzatura se presenti
028300141112     c                   clear                   fnlr98ds2
028400141112     c                   z-add     v03ANS        i982aas
028500141112     c                   z-add     V03LNP        i982lnp
028600141112     c                   z-add     V03NSP        i982nsp
028700141112     c                   z-add     V03NRS        i982nrs
028800141112     c                   call      'FNLR98R2'
028900141112     c                   parm                    fnlr98ds2
029000141112     c                   if        o982esito <> '0'
029100141114     c                   clear                   forzato
029200141112     c                   return
029300141114     c                   else
029400141114     c                   move      o982esito     forzato           1
029500141112     c                   endif
029600141112     c                   endif
029700101222      *  se ci sono errori  ed è richiamato deve uscire direttamente dal ciclo
029800110325     c   90              if        Richiamato = SI  and
029900110325     c                             Visual_Display <> SI
030000101222     c                   leave
030100101220     c                   end
030200110215     c* se non è richiamato
030300110215     c                   IF        Visual_Display = SI
030400110215     C* se non c'è l'errore ind.11 (scelto sia spedizione che barcode)
030500110215     c* azzero la spedizione o il barcode a seconda di come sto inserendo
030600110215     c* i riferimenti della spedizione
030700110215     c                   if        not *in11
030800110215     c                   if        combts = 'L' and v03nsp <> 0 and
030900110215     c                             v03pis <> ' '
031000110215     C                   MOVE      *year         V03ANS
031100110215     C                   Z-ADD     *ZEROS        V03LNP
031200110215     C                   Z-ADD     *ZEROS        V03NRS
031300110215     C                   Z-ADD     *ZEROS        V03NSP
031400110215     c                   end
031500110215     c                   if        combts <> 'L' and v03nsp <> 0 and
031600110215     c                             v03pis <> ' '
031700110215     C                   clear                   V03pis
031800110215     c                   end
031900110215     c                   end
032000110215     c                   endIF
032100110808      * altrimenti per errori deve riproporre il video
032200110808     c   90              iter
032300101223      *
032400030730     c*aggiorna
032500110808     c                   if         Visual_display = SI and
032600110808     c                              (*inkf or *inkh)
032700101222     c                                 or
032800101222     c                              Forza_aggiornamento = SI
032900101222     c                                 or
033000101222     c                              Visual_Display = NO
033100101222     c                              and Interrogazione = NO
033200101221     c                              and Richiamato = SI
033300101213     c*
033400101213      *                *-------------------*
033500030730     c                   exsr      sraggio
033600101213      *                *-------------------*
033700101222     c*
033800101222     c*  per errori in aggiornamento
033900101222     c   90              if        Richiamato = SI
034000101222     c                   leave
034100101221     c                   end
034200030730     c   90              iter
034300101222      *
034400101221      *  Senza errori, se richiamato,
034500101222      *  esce dal ciclo a questo punto confermando il corretto aggiornamento.
034600101220     c                   If        Richiamato = SI
034700031125     c                   move      '0'           o98esito
034800031125     c                   leave
034900031125     c                   endif
035000101222      **
035100101222      * se è arrivato qui è in interattivo da menù quindi ha aggiornato
035200101221     c* e re-inizializza  il video per riemetterlo
035300101213      *                *-------------------*
035400030730     c                   exsr      srinz
035500101213      *                *-------------------*
035600030730     c                   end
035700101213      *
035800030730     c                   enddo
035900101222      * -----------
036000110324     C     FINE          TAG
036100141114     c                   clear                   forzato
036200101213      *
036300031125     c                   movel(p)  fnlr98ds      kpjbu
036400101222     C*-----------------------------------------------------*
036500101222     C*    Operazioni iniziali
036600101222     C*-----------------------------------------------------*
036700101222     C     *INZSR        BEGSR
036800101222     C*
036900101222     C     *ENTRY        PLIST
037000101222     C                   PARM                    KPJBA
037100110125      * se richiamato da preparazione FNLR50R riceve i dati della spedizione
037200110125     c                   if        kpjbu <> *blank
037300110125     c                   movel     kpjbu         fnlr98ds
037400110125     c                   else
037500110125     c                   clear                   fnlr98ds
037600110125     c                   endif
037601160607     c                   if        i98video  = NO
037602160607     c                   else
037603160607     c                   open      fnlr98d
037604160607     c                   end
037700110125     c* se sono in interrogazione inizializzo la ds FNLR98DS
037800110125     c                   if        kcdaz = 'LR9A'
037900110125     c                   clear                   fnlr98ds
038000110125     c                   eval      I98INTERR ='S'
038100110125     c                   eval      I98STRCMT ='N'
038200110125     c                   eval      I98ALCDST ='N'
038300110125     c                   eval      I98CTLDST ='N'
038400110125     c                   end
038500101222     C*
038600101222      *** inizializza campi di ritorno                                                ***
038700101222     c                   move      '1'           o98esito
038800101222     c                   move      *blank        O98MSGERR
038900101222     c                   move      NO            Richiamato        1
039000101222     c                   move      SI            Visual_Display    1
039100101222     c                   move      SI            Controlla_Dist    1
039200101222     c                   move      NO            Interrogazione    1
039300101223     c                   eval      ALLOCA_Distinta = SI
039400101222     c                   move      *blank        Modalita          1
039500101222      ***                                                                             ***
039600101222     C* Inverto la data del giorno
039700101222     C                   TIME                    WHHDAT           14 0
039800101222     C                   MOVEL     WHHDAT        HHMMss            6 0          *ORA/MINUTI/sec
039900101222     C                   MOVE      WHHDAT        G02DAT
040000101222     C                   MOVE      *BLANKS       G02ERR
040100101222     C                   CALL      'XSRDA8'
040200101222     C                   PARM                    WLBDA8
040300101222     C                   Z-ADD     G02INV        DATEU             8 0
040400101222      *
040500101222     c                   setoff                                       37
040600110125     c                   eval      on = '0'
040700101222      *  se richiamato come interrogazione
040800101222     c                   if        i98interr = SI
040900101222     c                   eval      Interrogazione = SI
041000101222     c                   seton                                        37
041100110125     c                   else
041200110125     c                   eval      on = '1'
041300110125     c                   open      fnevb04l
041400110125     c                   open      fiarg01l
041500101222     c                   end
041600110125     c                   open      fnarb01l
041700101222      *
041800101222      *  controlli specifici sulla Distinta da Bypassare poichè fatti dal chiamante
041900101222     c                   if        I98CTLDST = NO
042000101222     c                   eval      Controlla_Dist = NO
042100101222     c                   end
042200101223      *
042300101223      *** allocazione Distinta
042400101223     c                   if        I98ALCDST = NO
042500101223     c                   eval      ALLOCA_Distinta = NO
042600101223     c                   end
042700101222      *
042800101222      *  se richiamato deve mostrare il video
042900101222     c                   if        i98video  = NO
043000101222     c                   eval      Visual_Display = NO
043100101222     c                   end
043200101222      *
043300101222     c*  modalità di utilizzo del programma (Togliere o Mettere)
043400101222     c                   eval      modalita = i98delins
043500101222      *
043600101222     c*** Se Richiamato i campi della spedizione ci devono essere
043700101222     c                   if        i98aas > 0  or
043800101222     c                             i98lnp > 0  or
043900101222     c                             i98nsp > 0  or
044000110301     c                             i98video = NO
044100101222     c                   move      SI            Richiamato
044200101222     c                   end
044300101222     c*
044400101222     c     *dtaara       define    §azute        azuteds
044500101222     c     *dtaara       define    §datiute      ddatiute
044600101222     C                   in(E)     *dtaara
044700101222     c*
044800101222     C                   IF        %Error  or  RSUT = *blanks
044900101222     C                   call      'TIBS34R'
045000101222     C                   parm                    Tibs34Ds
045100101222     C                   in        *dtaara
045200101222     c                   ENDIF
045300101222     c*
045400101222     c* controllo se p.o. partito non può + usare la funzione ma deve
045500101222     c* agire dal pgm di assegnaz./disassegnaz.
045600101222     C                   move      *date         dtaeur
045700101222     C                   move      dtaeur        dtaiso
045800101222     c                   move      dtaiso        oggin             8 0
045900101222     c                   move      oggin         oggi              8
046000101222     C     dutpou        CHAIN     AZORG01l
046100101222     c*
046200101222    1C                   IF        %found(azorg01l)
046300101222     C                   MOVEL     ORGde6        og146
046400101222     c                   movel     orgde8        og148
046500101222     c                   end
046600101222     c*
046700101222      * se c'è una data in organigramma
046800101222     c                   move      §ogdda        data_org          8 0
046900101222     C                   MOVE      *BLANK        test_Parallelo    1
047000101222     c                   if        §ogdda <> ' '
047100101222      * se siamo in fase di Parallelo prima di essere partiti
047200101222     c                   if        data_org = 20391231 or dateu < data_org
047300101222     C                   eval      test_Parallelo = 'S'
047400101222     c                   end
047500101222     c                   end
047600101222     c*
047700101222     C* Aggancio la £6 e recupero le linee a loro associate
047800101222     c                   clear                   trul06ds
047900101222     c                   eval      d06cod = '£6'
048000101222     c                   movel     dutpou        d06key
048100110125     c                   movel(p)  trul06ds      kpjbu
048200101222     c                   call      'TRUL06R'
048300101222     c                   parm                    kpjba
048400110125     c                   movel(p)  kpjbu         trul06ds
048500101222      *
048600101222     c* accendo o spengo l'indicatore 36 per gestire in seguito il campo
048700101222     c* dove andrà il barcode della spedizione
048800101222     C                   MOVEL     'L'           comBTS
048900101222     c     combts        comp      'L'                                    36
049000101222     c*
049100101222     c                   if        combts = 'L'
049200101222     c                   eval      v03f2 = 'F2=Manuale'
049300101222     c                   else
049400101222     c                   eval      v03f2 = 'F2=Pistola'
049500101222     c                   end
049600101222      *---------------------------*
049700101222      * CHIAVI                                                        *
049800101222      *---------------------------*
049900110107     C     K_autista     KLIST
050000101222     C                   KFLD                    apdtip
050100101222     C                   KFLD                    v03pdr
050200101222     c                   movel     'A'           apdtip
050300101222     C     KTAB          KLIST
050400101222     C                   KFLD                    tblkut
050500101222     C                   KFLD                    TBLCOD
050600101222     C                   KFLD                    TBLKEY
050700110107     C     Bolla_ARB     KLIST
050800101222     C                   KFLD                    ARBAAS
050900101222     C                   KFLD                    ARBLNP
051000101222     C                   KFLD                    ARBNRS
051100101222     C                   KFLD                    ARBNSP
051200110107     C     K_Distinta    KLIST
051300101222     C                   KFLD                    dstfgs
051400101222     C                   KFLD                    dstnpg
051500101222     C                   KFLD                    dstnfv
051600110107     C     Bolla_Prec    KLIST
051700101222     C                   KFLD                    LBLAAP
051800101222     C                   KFLD                    LBLLPP
051900101222     C                   KFLD                    LBLNRP
052000101222     C                   KFLD                    LBLNSP
052100101222     c                   eval      dstnpg = 4
052200110107     C     Evb_Bolla     KLIST
052300101222     C                   KFLD                    KAAS
052400101222     C                   KFLD                    KLNP
052500101222     C                   KFLD                    KNRS
052600101222     C                   KFLD                    KNSP
052700101222     C                   KFLD                    KCEV
052800101222     C*
052900110107     C     Anm_Bolla     KLIST
053000101222     C                   KFLD                    WAA4
053100101222     C                   KFLD                    ARBLNP
053200101222     C                   KFLD                    ARBNRS
053300101222     C                   KFLD                    ARBNSP
053400101222     C                   KFLD                    §§§CAA            3 0
053500101222     C*
053600101222     C     *LIKE         DEFINE    ARBIFP        KFGS
053700101222     C     *LIKE         DEFINE    ARBNDC        KNDC
053800101222     C     *LIKE         DEFINE    ARBDDC        KDDC
053900101222     C     *LIKE         DEFINE    EVBAAS        KAAS
054000101222     C     *LIKE         DEFINE    EVBLNP        KLNP
054100101222     C     *LIKE         DEFINE    EVBNRS        KNRS
054200101222     C     *LIKE         DEFINE    EVBNSP        KNSP
054300101222     C     *LIKE         DEFINE    EVBCEV        KCEV
054400101222     C     *LIKE         DEFINE    TBECOD        KCOD1
054500101222     C     *LIKE         DEFINE    TBEKE1        KKEY1
054501160316     C*
054502160316     C     KTBE          KLIST
054503160316     C                   KFLD                    KCOD1
054504160316     C                   KFLD                    KKEY1
054600101222     C*
054700101222     C                   MOVEL     'FNLR98R'     VIDPGM
054800101222     C                   MOVE      *year         V03ANS
054801160316     c* giro vedi pacco DPD
054803160316     C                   MOVEL     'NSD'         KCOD1
054804160316     C                   MOVEL     *BLANKS       KKEY1
054805160316     C                   MOVEL     '1'           KKEY1
054806160316     C     KTBE          CHAIN     TNTBE01L                           13
054807160316     C     *IN13         IFEQ      '0'
054808160316     C                   MOVEL     TBEUNI        DNSD
054809160316     C                   END
054900101222     C*
055000101222     C                   ENDSR
055100101220     c**********************************************************************
055200101220     C     srctr         BEGSR
055300101220     c**********************************************************************
055400101220     C                   SETOFF                                       90
055500101222     c                   eval       Forza_aggiornamento = 'N'
055600101220      *  Campi Inseriti a Video : Controllo di inserimento e correttezza
055700101221     c                   IF        Visual_Display = SI
055800101222     c* se il campo v03pis è impostato valorizzo i campi della spedizione
055900030620     c* solo se non sono stati impostati a mano o al giro di barcode che poi
056000030620     c* ha dato l'errore
056100030618     c                   clear                   ds_pis
056200030730     c                   if        v03pis <> ' '
056300030730     c                   movel     v03pis        ds_pis
056400030623     c                   if        v03nsp = 0 and
056500030620     c                             v03lnp = 0 and v03nrs = 0
056600030730     C                   MOVE      *year         V03ANS
056700030730     c                   move      ds_pisaas     v03ans
056800030618     c                   move      ds_pislnp     v03lnp
056900030618     c                   move      ds_pisnrs     v03nrs
057000030618     c                   move      ds_pisnsp     v03nsp
057100030623     c* controllo il check digits
057200030623     c                   exsr      srchkdgt
057300030730     c                   if        *in10
057400101220     c                   leaveSR
057500030618     c                   end
057600030623     c                   else
057700030730     c                   move      v03ans        com02             2 0
057800030623     c                   if        com02<>ds_pisaas or v03lnp<>ds_pislnp or
057900030623     c                             v03nrs<>ds_pisnrs or v03nsp<>ds_pisnsp
058000030730     c                   seton                                        1190
058100101220     c                   leaveSR
058200030623     c                   end
058300030623     c                   end
058400030623     c                   end
058500101221      *
058600030730     C* F2-Inserimento spedizione da manuale a pistola o viceversa
058700030730     c* serve per il posizionamento sulla spedizione o sul barcode
058800030730     C                   if        *inkb
058900080605     c                   if        combts = 'L'
059000080605     c                   movel     *blanks       combts
059100030703     c                   eval      v03f2 = 'F2=Pistola'
059200030703     c                   seton                                        90
059300030703     c                   setoff                                       7336
059400101220     c                   leaveSR
059500030703     c                   else
059600080605     c                   movel     'L'           combts
059700030703     c                   eval      v03f2 = 'F2=Manuale'
059800030703     c                   seton                                        907336
059900101220     c                   leaveSR
060000030703     c                   end
060100030730     C                   END
060200101221      *
060300030730     C*F7-INTERROGAZIONE BOLLE ARRIVI
060400030730     C                   if        *inkg
060500941109     C                   MOVEL     '2'           P36FLG
060600030730     C                   Z-ADD     V03ANS        P36AAS
060700941108     C                   Z-ADD     V03LNP        P36LNP
060800941108     C                   Z-ADD     V03NSP        P36NSP
060900941108     C                   Z-ADD     V03NRS        P36NRS
061000941109     C                   MOVEL     ' '           P36F03
061100941109     C     P36NSP        IFGT      0
061200941109     C                   MOVEL     '1'           P36FLG
061300941109     C                   END
061400110125     C                   MOVEL(p)  DSLR36        KPJBU
061500941108     C                   CALL      'FNLR36R'
061600911128     C                   PARM                    KPJBA
061700110125     C                   MOVEL(p)  KPJBU         DSLR36
061800941109     C     P36F03        IFNE      '1'
061900941215     C     V03NSP        ANDEQ     0
062000030730     C                   Z-ADD     P36AAS        V03ANS
062100941109     C                   Z-ADD     P36LNP        V03LNP
062200941109     C                   Z-ADD     P36NRS        V03NRS
062300941109     C                   Z-ADD     P36NSP        V03NSP
062400941109     C                   END
062500911128     C                   SETON                                        90
062600101220     c                   leaveSR
062700911128     C                   END
062800101220     c                   endIF
062900110124      ** Dopo il Video i campi sono stati impostati
063000110124      ** oppure  arrivano direttamente dai Parametri se richiamato
063100110124     c*
063200101222     C* Controllo se la spedizione è esistente o valida su video
063300101220     C     V03ANS        IFEQ      *ZEROS
063400101220     C                   SETON                                        0190
063500101221     c                   eval      O98msgERR = ERR(16)
063600101220     c                   leaveSR
063700101220     C                   END
063800101220     C     V03LNP        IFEQ      *ZEROS
063900101220     C                   SETON                                        0290
064000110301     c                   eval      O98msgERR = ERR(16)
064100101220     c                   leaveSR
064200101220     C                   END
064300101220     C     V03NSP        IFEQ      *ZEROS
064400101220     C                   SETON                                        0390
064500110301     c                   eval      O98msgERR = ERR(16)
064600101220     c                   leaveSR
064700101220     C                   END
064800101220      **
064900101220     C                   Z-ADD     V03ANS        ARBAAS
065000101220     C                   Z-ADD     V03LNP        ARBLNP
065100101220     C                   Z-ADD     V03NRS        ARBNRS
065200101220     C                   Z-ADD     V03NSP        ARBNSP
065300101220     C* AGGANCIO FNARB
065400110107     C     Bolla_ARB     CHAIN(n)  FNARB01L
065500101220      *   NON TROVATA LA BOLLA
065600101220     c                   if        not %found(fnarb01l)
065700101220     c                   seton                                        0590
065800101220     c                   eval      O98msgERR = ERR(05)
065900101220     c                   leaveSR
066000101220     c                   end
066100110124     c* Controllo se linea di arrivo abilitata
066200101220     C     ARBLNA        LOOKUP    LINT                                   70
066300101220     c                   if        not *in70
066400101220     C                   SETON                                        0490
066500101220     c                   eval      O98msgERR = ERR(04)
066600101220     c                   leaveSR
066700101220     c                   end
066800101220      **  DECODIFICHE da ARB
066900110104     c                   if        Modalita = Togliere
067000101220     c                   z-add     arbifp        v03ifp
067100101220     c                   z-add     arbndc        v03ndc
067200110104     c                   z-add     arbifp        dstfgs
067300110104     c                   z-add     arbndc        dstnfv
067400110124     c                   z-add     4             dstnpg
067500110104     c                   elseIf    Modalita = Mettere
067600110104      * se metto su ARB NON c'è la distinta
067700110104     c                   z-add     i98fgs        v03ifp
067800110105     c                   z-add     i98fgs        dstfgs
067900110104     c                   z-add     i98ndc        v03ndc
068000110124     c                   z-add     i98npg        dstnpg
068100110105     c                   z-add     i98ndc        dstnfv
068200110104     c                   end
068300101220      *   Data Distinta
068400101220     c                   if        arbddc <> 0
068500101220     c                   move      arbddc        dtaiso
068600101220     c                   move      dtaiso        dtaeur
068700101220     c                   move      dtaeur        v03ddc
068800101220     c                   else
068900101220     c                   z-add     0             v03ddc
069000101220     c                   end
069100101220     C* decodifica mattina/pomeriggio
069200101220     c                   clear                   v03fpp
069300110107     C     k_Distinta    CHAIN     fidst09L
069400101220     c                   if        %found(fidst09l)
069500110104     c                   if        v03ddc  = 0
069600110104     c                   move      dstdfv        dtaiso
069700110104     c                   move      dtaiso        dtaeur
069800110104     c                   move      dtaeur        v03ddc
069900110104     c                   end
070000110104     c                   eval      ddstflr = dstflr
070100101220     c                   if        dstfpp = 'M'
070200101220     C                   eval      v03fpp = 'Mattino'
070300101220     c                   elseIf    dstfpp = 'P'
070400101220     C                   eval      v03fpp = 'Pomeriggio'
070500101220     c                   end
070600110104     C* decodifica padroncino    Autista
070700110104     c                   move      arbpdc        v03pdr
070800110104     c                   if        v03pdr  = 0
070900110104     c                   move      dstpdr        v03pdr
071000110104     c                   end
071100110104     c                   clear                   v03rsc
071200110107     C     K_autista     CHAIN     fiapd01L
071300110104     c                   if        %found(fiapd01l)
071400110104     C                   MOVEl(p)  APDRSC        V03rsc
071500110104     c                   end
071600110203     c                   endIF
071700110804     c* forzatura perfect order solo per gestione display
071800130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
071900130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
072000110804     c                   IF        Visual_Display = SI
072100110808     c                             and okprfo = ' '
072200130221     c                             and arbxco > '0' and arbntc = 0
072300130221     c****************             and arbxco <> ' ' and arbntc = 0
072400110808     c                   seton                                        9044
072500111020     c                   if        combts = 'L'
072600111020     c                   eval      savpis = v03pis
072700111020     c                   else
072800110804     c                   eval      savans = v03ans
072900110804     c                   eval      savlnp = v03lnp
073000110804     c                   eval      savnrs = v03nrs
073100110804     c                   eval      savnsp = v03nsp
073200110804     c                   end
073300111020     c                   end
073400101220      * ------------------------------
073500101220      *    Controlli specifici per TOGLI o METTI
073600101220      * ------------------------------
073700101221     c                   if        Modalita = Togliere
073800101220     c                   exsr      srctr_Togli
073900101221     c                   elseIf    Modalita = Mettere
074000101220     c                   exsr      srctr_Metti
074100101220     c                   end
074200110124     c   90              leaveSR
074300101220      *
074400101220     c                   endsr
074500101220     c**********************************************************************
074600101220     C     srctr_Togli   BEGSR
074700101220     c**********************************************************************
074800101220      *  SPECIFICI per TOGLIERE una spedizione da una DISTINTA
074900101220      * Per Togliere:
075000101220     C* controllo la distinta che deve essere presente sulla Bolla
075100101220     c                   if        arbndc = 0
075200030730     c                   z-add     0             v03ifp
075300030730     c                   z-add     0             v03ndc
075400030730     c                   z-add     0             v03ddc
075500030730     c                   z-add     0             v03pdr
075600030730     c                   clear                   v03rsc
075700080415     c                   clear                   v03fpp
075800110324      * se non c'è NON SI PUO' TOGLIERE .........
075900110325     c                   seton                                        1990
076000101221     c                   eval      O98msgERR = ERR(19)
076100101220     c                   leaveSR
076200101217     c                   end
076300101220      **  se richiamato, PER Togliere
076400101220      **       La distinta passata deve corrispondere a quella della bolla
076500101220     c                   If        Richiamato = SI
076600101220     c                   z-add     0             parmNDC           7 0
076700110103     c                   z-add     I98NDC        parmNDC
076800101220     c                   if          parmNDC <> arbNDC
076900110324     c                   seton                                        0690
077000101220     c                   eval      O98msgERR = ERR(11)
077100101220     c                   leaveSR
077200101220     c                   end
077300101220     c                   endIf
077400911017     C                   ENDSR
077500101220     c**********************************************************************
077600101220     C     srctr_Metti   BEGSR
077700101220     c**********************************************************************
077800101223      **   deve controllare che la Bolla non sia già legata
077900101223      **    ad un'altra distinta
078000101223     c                   if          arbNDC <> 0
078100110324     c                   seton                                        0790
078200101220     c                   eval      O98msgERR = ERR(13)
078300101220     c                   leaveSR
078400101220     c                   endIf
078500110107     C* passa se c'è anche l'ARG
078600110107     c                   clear                   fiargds$
078700110107     c     Bolla_ARB     chain(n)  fiarg01l
078800110107     c                   if        %Found(Fiarg01L)
078900110107     c                   eval      fiargds$ = fiargds
079000110107     c                   end
079001160316     c* se bolla vedi pacco dpd e aut NON è fittizio errore
079002160316     c                   if        argcgi = §nsdcgi
079003160316     c                             and apdpdd <> 'S'
079005160316     c                   seton                                        9990
079006160316     c                   eval      O98msgERR = ERR(21)
079007160316     c                   leaveSR
079008160316     c                   end
079009160316     c
079100101223     C*   esternizzata Routine di controlli
079200101223     C*      sulla Consegnabilità della Bolla
079300141105     c                   clear                   Fnlrq4DS
079400141103     c                   if        kcdaz <> 'LG80' and kcdaz <> 'LRQ9'
079500101223     c                   z-add     v03ans        LRQ4aasI
079600101223     c                   z-add     v03lnp        LRQ4lnpI
079700101223     c                   z-add     v03nrs        LRQ4nrsI
079800101223     c                   z-add     v03nsp        LRQ4nspI
079900101223     c                   z-add     dstdfv        LRQ4ddcI
080000110105     c                   eval      fnarbds$ = fnarbds
080100110125     c                   movel(P)  Fnlrq4DS      kpjbu
080200101223     c                   call      'FNLRQ4R'
080300101223     c                   parm                    kpjba
080400110105     c                   parm                    fnarbds$
080500110107     c                   parm                    fiargds$
080501160509     c                   parm                    fidstds
080600130429     c                   movel(p)  kpjbu         Fnlrq4DS
080700130802     c                   endif
080800110124      * imposta l'errore
080900101223     c                   if        LRQ4ERRO <> *blank
081000101223     c                   eval      O98msgERR = LRQ4MSGO
081100110324     c*************      seton                                          90
081200110324     c                   seton                                        0890
081300110324     c                   leaveSR
081400101223     c                   end
081500101223     C*
081600101223     C                   ENDSR
081700080416     C*-----------------------------------------------------*
081800080416     C* pgm per forzare modifica distinta fase CUS          *
081900080416     C*-----------------------------------------------------*
082000101214     C     srctl_dist    BEGSR
082100101213      *
082200101223     c                   clear                   FnlrQ3ds
082300101223     c                   eval      LRQ3NPGI = dstnpg
082400101223     c                   eval      LRQ3FGSI = dstfgs
082500101223     c                   eval      LRQ3NDCI = dstnfv
082600101223     c                   eval      LRQ3ALCI = 'N'
082700101223     c                   if        ALLOCA_Distinta = SI
082800101223     c                   eval      LRQ3ALCI = 'S'
082900101223     c                   end
083000110125     c                   movel(p)  Fnlrq3DS      kpjbu
083100101223     c                   call      'FNLRQ3R'
083200101223     c                   parm                    kpjba
083300110104     c                   parm                    fidstds
083400110125     c                   movel(p)  kpjbu         Fnlrq3DS
083500101223      *
083600101223     c                   if        LRQ3ERRO <> *blank
083700101223     c                   eval      O98msgERR = LRQ3MSGO
083800110325     c                   seton                                        9006
083900101223     c                   leaveSR
084000101223     c                   end
084100101214     C                   ENDSR
084200911018     C*-----------------------------------------------------*
084300030623     C* controllo check digits
084400911018     C*-----------------------------------------------------*
084500030623     C     srchkdgt      BEGSR
084600030623     c                   movel     ds_pis        barcode
084700030623     C                   clear                   trul28ds
084800030623     c                   eval      i28mod = 'BAR'
084900030623     c                   eval      i28cod = barcode
085000030623     c                   call      'TRUL28R1'
085100030623     c                   parm                    trul28ds
085200030623     c* se reperito barcode imposto i campi in stampa
085300030623     c                   if        o28err <> *blanks or  o28cod <> ds_pis
085400090904     c                   seton                                        1090
085500030623     c                   end
085600101223      **
085700030623     C                   ENDSR
085800030730     C*-----------------------------------------------------*
085900030730     C* sraggio - aggiorno FNARB, anomalie ed eventi        *
086000030730     C*-----------------------------------------------------*
086100030730     c     sraggio       begsr
086200110105     c                   move      'N'           esegue_ROLBK      1
086300110125     c* Controllo/alloco distinta solo se richiesto
086400110125     c                   if        Controlla_Dist = SI
086500110125     c                   exsr      srctl_dist
086600110125     c   90              leaveSR
086700110125     c                   end
086800101213      **
086900030730     C* AGGANCIO FNARB
087000110107     C     Bolla_ARB     CHAIN(e)  FNARB01L
087100101214     c                   if        %error
087200101214      * Bolla non allocabile per l'aggiornamento
087300101214     c                   eval      O98msgERR = ERR(12)
087400110324     c*************      seton                                          90
087500110324     c                   seton                                        1690
087600101214     c                   leavesr
087700101214     c                   end
087800101220      **  Solo se richiamata x TOGLIERE:
087900101221     c                   if        Modalita = Togliere
088000030730     C* aggiorno le eventuali anomalie sulla bolla
088100030730     C                   EXSR      ANOM
088200030730     C* tolgo l'evento MIC o creo il NIC
088300980309     C                   EXSR      ANNMIC
088400110107     C                   Z-ADD     *ZEROS        ARBNSS
088500110107     C                   Z-ADD     *ZEROS        ARBIFP
088600110107     C                   Z-ADD     *ZEROS        ARBNDC
088700110107     C                   Z-ADD     *ZEROS        ARBDDC
088800110107     C                   Z-ADD     *ZEROS        ARBSTP
088900110107     C                   Z-ADD     *ZEROS        ARBNGD
089000110107     C                   Z-ADD     *ZEROS        ARBPDC
089100101222     c                   end
089200101222     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
089300101220     c* deve andare a togliere la distinta dall'orm
089400101220     c                   exsr      srorm
089500101214     C                   UPDATE    FNARB000
089600110124     c* aggiorna pesi/volumi/fermate assegnate x fidst00f solo se
089700110124     c* richiamato con allocazione distinta
089800101222     c                   if        §ogdda <> ' '
089900101223     c                   exsr      srfiarg
090000110124     c                   if        *in90
090100110124     c                   move      'S'           esegue_ROLBK
090200110124     c                   else
090300101223     c                   if        ALLOCA_Distinta = SI
090400071002     c                   exsr      srfidg37
090500110107     c   90              move      'S'           esegue_ROLBK
090600101223     c                   end
090700071002     c                   end
090800110124     c                   end
090900101222      * il commit se il programma è richiamato in cascata da altri
091000101222      *   può essere eseguito dall'esterno da altri programmi.
091100101213     c                   if        I98STRCMT = ' '
091200110105     c                   if          esegue_ROLBK = 'N'
091300110105     c                   COMMIT
091400110105     c                   else
091500110105     c                   ROLBK
091600110105     c                   end
091700101214     c                   end
091800101213      *
091900080416     c* richiamo il pgm per aggiornare i dati per PDA
092000101222     c                   if        §OGPDAcon <> ' ' and
092100080416     c                             (dstpda =  'C' or dstpda = 'E' or
092200101222     c                             §DSTTSTPDA= 'C' or
092300101222     c                             §DSTTSTPDA= 'E')
092400080506     c                   clear                   fidg42ds
092500110308     c                   if        Modalita = Togliere
092600080506     c                   eval      co42tla = 'A'
092700110308     c                   elseif    modalita = Mettere
092800110308     c                   eval      co42tla = 'I'
092900110308     C* AGGANCIO FNARB
093000110308     C     Bolla_ARB     CHAIN(n)  FNARB01L
093100110308     c                   end
093200080416     c                   exsr      srpda
093300080416     c                   end
093400101220      *
093500030730     c                   endsr
093600070419     C*-----------------------------------------------------*
093700070517     C* aggiorna pesi/volumi/fermate assegnate x fidst00f   *
093800070419     C*-----------------------------------------------------*
093900070517     C     srfidg37      BEGSR
094000070419     c*
094100070517     c                   clear                   fidg37ds
094200101223     c                   eval      D37FGSI = dstfgs
094300101223     c                   eval      D37DFVI = dstdfv
094400101223     c                   eval      D37NFVI = dstnfv
094500070517     c                   eval      D37cmti = 'N'
094600110125     c                   movel(p)  fidg37ds      kpjbu
094700070517     c                   call      'FIDG37R'
094800070517     c                   parm                    kpjba
094900110125     c                   movel(p)  kpjbu         fidg37ds
095000101220      *
095100101214     c                   if        D37ERRO <> ' '
095200101215      * Distinta non allocabile per l'aggiornamento
095300101215     c                   eval      O98msgERR = ERR(06)
095400110324     c*************      seton                                          90
095500110324     c                   seton                                        0690
095600101214     c                   leavesr
095700101214     c                   end
095800070419     c*
095900070419     C                   ENDSR
096000980309     C*-----------------------------------------------------*
096100980309     C* ANNMIC: Annullo evento di  "Messa in consegna"      *
096200980309     C*-----------------------------------------------------*
096300980309     C     ANNMIC        BEGSR
096400030730     C                   move      *date         dtaeur
096500030730     C                   move      dtaeur        dtaiso
096600090605     C* Controllo se ho scritto evento MIC
096700110107     C     Bolla_ARB     CHAIN     FNLBL01L                           87
096800090605     C*
096900090605     C     *IN87         IFEQ      '0'
097000090605     C     ARBLNA        ANDEQ     LBLLAP
097100110107     C     Bolla_Prec    CHAIN     FNARB01L                           30
097200090605     C     ARBCCA        ifeq      '2'
097300090605     C     ARBCCA        oreq      '6'
097400090605     C                   SETON                                        87
097500090605     C                   END
097600090827     c* manuale
097700090827     C                   Z-ADD     V03ANS        ARBAAS
097800090827     c                   if        v03nsp <> 0
097900090605     C                   Z-ADD     V03LNP        ARBLNP
098000090605     C                   Z-ADD     V03NRS        ARBNRS
098100090605     C                   Z-ADD     V03NSP        ARBNSP
098200090827     c* pistola
098300090827     c                   else
098400090827     c                   movel     v03pis        ds_pis
098500090827     c                   move      ds_pislnp     arblnp
098600090827     c                   move      ds_pisnrs     arbnrs
098700090827     c                   move      ds_pisnsp     arbnsp
098800090827     c                   end
098900110107     C     Bolla_ARB     CHAIN     FNARB01L                           30
099000090605     C                   END
099100090605     C*
099200090605     C     *IN87         IFEQ      '0'
099300090605     C     ARBLNA        ANDEQ     LBLLAP
099400090605     C                   Z-ADD     LBLAAP        KAAS
099500090605     C                   Z-ADD     LBLLPP        KLNP
099600090605     C                   Z-ADD     LBLNRP        KNRS
099700090605     C                   Z-ADD     LBLNSP        KNSP
099800090605     C                   ELSE
099900090605     C                   Z-ADD     ARBAAS        KAAS
100000090605     C                   Z-ADD     ARBLNP        KLNP
100100090605     C                   Z-ADD     ARBNRS        KNRS
100200090605     C                   Z-ADD     ARBNSP        KNSP
100300090605     C                   END
100400980309     C                   MOVEL     'MIC'         KCEV
100500110107     C     Evb_Bolla     SETGT     FNEVB04L
100600110107     C     Evb_Bolla     READPE    FNEVB04L                               33
100700980309     C     *IN33         IFEQ      '0'
100800101223     C     dstDFV        ANDEQ     EVBDEV
100900101222     C* Se MIC esiste e la data della distinta è la stessa
101000101222     C* dell'evento ... controllo se è già trasmesso
101100980311     C     EVBDTR        IFNE      0
101200990803     C                   MOVEL     EVBDEV        WDTMIC           12 0
101300980311     C                   MOVE      EVBHEV        WDTMIC
101400101222     C* se l'evento di messa in consegna è stato trasmesso
101500101222     C* controllo se esiste già un evento NIC tolto dalla
101600980311     C* consegna
101700980311     C                   MOVEL     'NIC'         KCEV
101800110107     C     Evb_Bolla     SETGT     FNEVB04L
101900110107     C     Evb_Bolla     READPE    FNEVB04L                               33
102000031021     C* SE HO TROVATO EVENTO NIC
102100030730     C     *IN33         IFEQ      '0'
102200101223     C     dstDFV        ANDEQ     EVBDEV
102300101222     C* Se non è stato trasmesso aggiorno la data e l'ora di
102400031021     C* di immissione evento con quella del giorno + altri dati
102500031021     C     EVBDTR        IFEQ      0
102600031021     C                   move      dtaiso        EVBDTV
102700031021     C                   TIME                    EVBORV
102800031021     C                   Z-ADD     dutpou        EVBFLE
102900031021     C                   Z-ADD     DUTCOU        EVBCDU
103000101223     C     dstFPP        IFEQ      'M'
103100031021     C                   MOVEL     0800          EVBHEV
103200031021     C                   ELSE
103300031021     C                   MOVEL     1200          EVBHEV
103400031021     C                   END
103500031021     C                   MOVEL     *BLANKS       EVBDTC
103600101214      *
103700031021     C                   UPDATE    FNEVB000
103800101214      *
103900101222     C* Se è stato trasmesso controllo se non ha data più
104000031021     C* alta del MIC
104100031021     C                   ELSE
104200031021     C                   MOVEL     EVBDEV        WDTNIC           12 0
104300031021     C                   MOVE      EVBHEV        WDTNIC
104400031021     C     WDTNIC        IFLE      WDTMIC
104500101222     C* Se la data non è più alta scrivo un nuovo NIC
104600031021     C                   CLEAR                   FNEVB000
104700031021     C                   MOVEL     'NIC'         EVBCEV
104800031021     C                   move      dtaiso        EVBDTV
104900031021     C                   TIME                    EVBORV
105000031021     C                   Z-ADD     dutpou        EVBFLE
105100031021     C                   Z-ADD     DUTCOU        EVBCDU
105200031021     C                   Z-ADD     KAAS          EVBAAS
105300031021     C                   Z-ADD     KLNP          EVBLNP
105400031021     C                   Z-ADD     KNRS          EVBNRS
105500031021     C                   Z-ADD     KNSP          EVBNSP
105600101223     C                   Z-ADD     dstDFV        EVBDEV
105700101223     C     dstFPP        IFEQ      'M'
105800031021     C                   MOVEL     0800          EVBHEV
105900031021     C                   ELSE
106000031021     C                   MOVEL     1200          EVBHEV
106100031021     C                   END
106200131104     c                   clear                   Devb01
106300131104     C                   move      *zeros        §NOTNDC
106400131104     C                   move      *zeros        §NOTPDC
106500131104     C                   move      ARBNDC        §NOTNDC
106600131104     C                   move      ARBPDC        §NOTPDC
106700131104     C                   MOVEL     Devb01        EVBNOT
106800031021     C                   MOVEL     *BLANKS       EVBDTC
106900101214      *
107000031021     C                   WRITE     FNEVB000
107100101214      *
107200031021     C                   END
107300031021     C*
107400031021     C                   END
107500031021     C* SE NON ESISTE EVENTO NIC avente la stessa data evento della
107600031021     C* distinta lo scrivo
107700030730     C                   ELSE
107800031021     C                   CLEAR                   FNEVB000
107900031021     C                   MOVEL     'NIC'         EVBCEV
108000031021     C                   move      dtaiso        EVBDTV
108100031021     C                   TIME                    EVBORV
108200031021     C                   Z-ADD     dutpou        EVBFLE
108300031021     C                   Z-ADD     DUTCOU        EVBCDU
108400031021     C                   Z-ADD     KAAS          EVBAAS
108500031021     C                   Z-ADD     KLNP          EVBLNP
108600031021     C                   Z-ADD     KNRS          EVBNRS
108700031021     C                   Z-ADD     KNSP          EVBNSP
108800101223     C                   Z-ADD     dstDFV        EVBDEV
108900101223     C     dstFPP        IFEQ      'M'
109000031021     C                   MOVEL     0800          EVBHEV
109100031021     C                   ELSE
109200031021     C                   MOVEL     1200          EVBHEV
109300031021     C                   END
109400131104     c                   clear                   Devb01
109500131104     C                   move      *zeros        §NOTNDC
109600131104     C                   move      *zeros        §NOTPDC
109700131104     C                   move      ARBNDC        §NOTNDC
109800131104     C                   move      ARBPDC        §NOTPDC
109900131104     C                   MOVEL     Devb01        EVBNOT
110000031021     C                   MOVEL     *BLANKS       EVBDTC
110100101214      *
110200031021     C                   WRITE     FNEVB000
110300980311     C*
110400030730     C                   END
110500101222     C* Se MIC non è stato trasmesso annullo fisicamente record
110600980311     C                   ELSE
110700980309     C* annullo fisicamente l'evento se da trasmettere
110800101214      *
110900990803     C  N33              DELETE    FNEVB04L
111000101214      *
111100030730     C                   END
111200030730     C                   END
111300980309     C*
111400980309     C                   ENDSR
111500930623     C*-----------------------------------------------------*
111600930623     C* GESTIONE ANOMALIE DELLA BOLLA                       *
111700930623     C*-----------------------------------------------------*
111800930623     C     ANOM          BEGSR
111900930623     C*
112000960415     C                   CLEAR                   DSLR33
112100960415     C                   MOVE      4             D33FSC
112200030730     C                   MOVE      v03ddc        Dtaeur
112300030730     C                   MOVE      dtaeur        Dtaiso
112400030730     C                   MOVE      dtaiso        D33DCH
112500960415     C*
112600990519     C                   MOVE      ARBAAS        WAA4              4 0
112700101222     C                   Z-ADD     135           §§§CAA
112800110107     C     Anm_Bolla     SETLL     FNANM01L                           34
112900930624     C     *IN34         DOWEQ     '0'
113000110107     C     Anm_Bolla     READE     FNANM01L                               34
113100960415     C     *IN34         IFEQ      *OFF
113200960415     C                   Z-ADD     ANMNR1        D33NRR
113300960415     C                   CALL      'FNLR33R'
113400960415     C                   PARM                    DSLR33
113500960415     C                   MOVE      '1'           W33R              1
113600960415     C                   END
113700960415     C                   ENDDO
113800950918     C*
113900101222     C                   Z-ADD     150           §§§CAA
114000110107     C     Anm_Bolla     SETLL     FNANM01L                           34
114100950915     C     *IN34         DOWEQ     '0'
114200110107     C     Anm_Bolla     READE     FNANM01L                               34
114300960415     C     *IN34         IFEQ      *OFF
114400960415     C                   Z-ADD     ANMNR1        D33NRR
114500960415     C                   CALL      'FNLR33R'
114600960415     C                   PARM                    DSLR33
114700960415     C                   MOVE      '1'           W33R              1
114800960415     C                   END
114900960415     C                   ENDDO
115000930623     C*
115100930623     C                   ENDSR
115200100514     C*--------------------------------------------------------------*
115300101222     C*  sr7r  particolarità consegna
115400100514     C*--------------------------------------------------------------*
115500100514     C     sr7r          BEGSR
115600100514     c*
115700100514     C                   eval      tblcod = '7R'
115800100514     C                   eval      tblkey = arbgma
115900100514     c                   eval      tblkut = 1
116000100514     C     ktab          chain     Tabel00f
116100100514     c                   if        %found(tabel00f)
116200100514     C                   MOVEL     TBLUNI        DS7R
116300100514     c                   end
116400100514     C*
116500100514     C                   ENDSR
116600030826     C*-----------------------------------------------------*
116700101222     C*  toglie sull'orm x particolarità RC
116800030826     C*-----------------------------------------------------*
116900030826     C     srorm         BEGSR
117000101222      *
117100101222      * Tolgo anche il Ritiro Contestuale alla consegna
117200100514     c                   clear                   ds7r
117300100514     c                   if        arbgma <> ' '
117400100514     c                   exsr      sr7r
117500100514     c                   end
117600101222     c                   if        §7RRC ='S'
117700030826     c                   clear                   fnlr99ds
117800030826     c                   eval      i99tla = 'L'
117900030826     c                   eval      i99aas = arbaas
118000030826     c                   eval      i99lnp = arblnp
118100030826     c                   eval      i99nrs = arbnrs
118200030826     c                   eval      i99nsp = arbnsp
118300071001     c                   move      v03ifp        i99fgs
118400080930     c                   z-add     arbndc        i99ndc
118500030826     c                   eval      i99ddc = arbddc
118600071001     c                   eval      i99CMT = 'N'
118700071001     c                   eval      i99CoMiT = '1'
118800101222      *
118900101222     c                   if        modalita = Togliere
119000101222     c                   eval      i99fao = 390
119100101222     c                   elseif    modalita = Mettere
119200101222     c                   eval      i99fao = 400
119300101222     c                   eval      i99npg = 4
119400101222     c                   eval      i99nftl= 99999
119500101222     c                   end
119600101222      *
119700030826     c                   call      'FNLR99R'
119800030826     c                   parm                    kpjba
119900030826     c                   parm                    fnlr99ds
120000030826     c                   end
120100101222      *
120200030826     C                   ENDSR
120300080416     C*----------------------------------------------------*
120400080416     C* se pda e tolgo o metto spedizione e fase PDC richiamo pgm per
120500080416     c* aggiornare i dati al PDA
120600080416     C*----------------------------------------------------*
120700080416     C     srpda         BEGSR
120800080416     c                   eval      co42FGS = dstfgs
120900080416     C                   eval      co42NDC = dstnfv
121000080416     c                   eval      co42ddc = dstdfv
121100080801     c                   eval      co42aas =  arbaas
121200080801     c                   eval      co42lnp  = arbLNP
121300080801     c                   eval      co42nrs  = arbNRS
121400080801     c                   eval      co42nsp  = arbNSP
121500110308     c                   eval      fnarbds$ = fnarbds
121600080416     c                   movel(p)  fidg42ds      kpjbu
121700080416     c                   call      'FIDG42R'
121800080416     c                   parm                    kpjba
121900110308     c                   parm                    fnarbds$
122000101213     C*
122100080416     C                   ENDSR
122200070531     C*-----------------------------------------------------*
122300101217     C* toglie distinta sul FIARG oppure la mette
122400070531     C*-----------------------------------------------------*
122500070531     C     srfiarg       BEGSR
122600120203     C                   CLEAR                   FIARGDS
122700101213     C*
122800101217     c     ARG_di_nuovo  tag
122900101217     C*
123000110107     c     Bolla_ARB     chain(e)  fiarg01l
123100110104     C*  x errore
123200110104     c                   eval      ARG_Allocato = NO
123300101217     c                   if        %error
123400101217      *    Problemi durante l'aggiornamento --> eseguire ROLLBACK
123500101217     c                   clear                   trul82ds
123600101222     c                   eval      ul82§rrn = arg_nrr
123700101222     c                   eval      ul82§fil = 'FIARG01L'
123800101222     c                   eval      ul82§win = 'S'
123900101222     c                   eval      ul82§f7  = 'S'
124000101222     c                   eval      ul82§num = 2
124100101222     c                   eval      ul82§att = 2
124200101222     c                   eval      ul82§mss = 'Si sta bloccando la Distinta: '
124300110104     c                             + %editc(dstnfv:'Z') +
124400101217     c                             ' SI PREGA DI USCIRE dal lavoro|'
124500101222     c                   Eval      UL82§msw = 'La Distinta '
124600110103     c                             + %editc(I98NDC:'Z') +  ' '
124700101222     c                             + ' non è manutenzionabile.'
124800101217      *
124900101217      * chiamo il pgm che manda il messaggio info all'utente
125000101217     c                   call(e)   'TRUL82R'
125100101217     c                   parm                    trul82ds
125200101217     C*
125300101222      * se non più Allocata  riaggancia il record
125400101222     c                   if        ul82§sts <> 'A'
125500101217     c                   goto      ARG_di_nuovo
125600110104     c                   else
125700110104     c                   eval      ARG_Allocato = SI
125800110104      * Distinta non disponibile.....
125900110104     c                   eval      O98msgERR = ERR(12)
126000110324     c*************      seton                                          90
126100110324     c                   seton                                        0690
126200110104     c                   leavesr
126300110104     c                   endif
126400101220     C*
126500101217     c                   end
126600101217     C*
126700101217     C*----------- Se rimane in errore invia segnalazioni (MSG di errore)
126800101221     c                   if        not %Found(fiarg01l) and Modalita = Togliere
126900101217     c                                or
127000110104     c                             %Found(fiarg01l) and Modalita = Mettere and
127100110125     c                             argNDC > 0  and test_Parallelo = 'S'
127200101222      * Distinta non più disponibile.....
127300110104     c                   eval      O98msgERR = ERR(20)
127400110324     c*************      seton                                          90
127500110324     c                   seton                                        0690
127600101217     c                   leavesr
127700101217     c                   end
127800101217     C*
127900101220     C*----- Quindi prosegue:
128000101220     C*- in aggiornamento
128100101217     c                   if        %found(fiarg01l)
128200110104     C* se deve togliere
128300110104     c                   if         Modalita = Togliere
128400100202     c                   eval      dargflo = argflo
128500101222     c                   clear                   §argattesa
128600100202     c                   eval      argflo = dargflo
128700101217      *
128800070531     C                   clear                   argfgs
128900070531     C                   clear                   argNDC
129000070531     C                   clear                   argddc
129100070531     C                   clear                   argstp
129200070531     C                   clear                   argpdc
129300070531     c                   clear                   argnftl
129400070531     c                   clear                   ARGDTVDIS
129500070531     c                   clear                   ARGHVDIS
129600070531     c                   clear                   argslb
129700110104     c                   elseIf     Modalita = Mettere
129800110104     C* se deve Mettere
129900110104     c                   move      arbLNA        ARGLNA
130000110104     c                   move      I98fgs        ARGFGS
130100110104     c                   move      I98ndc        ARGNDC
130200110104     c                   eval      ARGPDC     =  DSTPDR
130300110104     c                   eval      ARGDDC     =  DSTDFV
130400110104     c                   eval      ARGSTP     =  0
130500110104     c                   eval      ARGNFTL    =  99999
130600110104     c                   eval      ARGSLB     =  0
130700110104     c                   eval      ARGDTVDIS  = Dateu
130800110104     c                   eval      ARGHVDIS   = HHMMss
130900110104     c                   end
131000101220     c                   update    fiarg000
131100110104     C* se deve Mettere
131200101217     c                   else
131300110104      * in inserimento scrive ARG poi aggiorna ARB con la routine di quadratura
131400110104     c                   move      arbAAS        ARGAAS
131500110104     c                   move      arbLNP        ARGLNP
131600110104     c                   move      arbNRS        ARGNRS
131700110104     c                   move      arbNSP        ARGNSP
131800110104     c                   move      arbLNA        ARGLNA
131900110104     c                   move      I98fgs        ARGFGS
132000110104     c                   move      I98ndc        ARGNDC
132100110104     c                   eval      ARGPDC     =  DSTPDR
132200110104     c                   eval      ARGDDC     =  DSTDFV
132300110104     c                   eval      ARGSTP     =  0
132400110104     c                   eval      ARGNFTL    =  99999
132500110104     c                   eval      ARGSLB     =  0
132600110104     c                   eval      ARGDTVDIS  = Dateu
132700110104     c                   eval      ARGHVDIS   = HHMMss
132800110104     c                   write     fiarg000
132900110105     C*
133000110105     c                   endif
133100101217     c*
133200110105     C* esegue aggiornamento di ARB
133300110105     c                   clear                   fidg23ds
133400110105     c                   eval      d23CMTI = 'N'
133500110105     c                   eval      d23FGSI = I98fgs
133600110105     c                   eval      d23NFTL = 99999
133700110105     c                   eval      d23DFVI = dstDFV
133800110105     c                   eval      d23NFDI = I98ndc
133900110105     c                   eval      d23NFAI = I98ndc
134000110105     c                   eval      d23FPPI = dstFPP
134100110125     c                   movel(p)  fidg23ds      kpjbu
134200110105     c                   call      'FIDG23C'
134300110105     c                   parm                    kpjba
134400110125     c                   movel(p)  kpjbu         fidg23ds
134500110105      * se c'è stato un errore
134600110105     c                   if        d23erro <> *blanks
134700110105      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
134800110105     c                   eval      O98msgERR = ERR(07)
134900110324     c*************      seton                                          90
135000110324     c                   seton                                        0690
135100110105     c                   leavesr
135200110105     c                   end
135300101220      *
135400070531     c                   endsr
135500101220     c**********************************************************************
135600101220     C     srinz         BEGSR
135700101220     c**********************************************************************
135800101220     C* Pulisco i campi che devo vidualizzare nel fm2
135900101220     C*
136000101220     C                   clear                   V03pis
136100101220     C                   MOVE      *year         V03ANS
136200101220     C                   Z-ADD     *ZEROS        V03LNP
136300101220     C                   Z-ADD     *ZEROS        V03NRS
136400101220     C                   Z-ADD     *ZEROS        V03NSP
136500101220     C                   Z-ADD     *ZEROS        V03ifp
136600101220     C                   Z-ADD     *ZEROS        V03ndc
136700101220     C                   Z-ADD     *ZEROS        V03ddc
136800101220     C                   Z-ADD     *ZEROS        V03pdr
136900101220     C                   MOVEL     *blanks       V03rsc
137000101220     C                   MOVEL     *blanks       V03fpp
137100101220      *
137200101220     c* richiamo il pgm per dati a PDA x chiudere in lr
137300101222     c                   if        §OGPDAcon <> ' ' and
137400101220     c                             (dstpda =  'C' or dstpda = 'E' or
137500101222     c                             §DSTTSTPDA= 'C' or
137600101222     c                             §DSTTSTPDA= 'E')
137700101220     c                   clear                   fidg42ds
137800101220     c                   eval      co42tla = 'C'
137900101220     c                   exsr      srpda
138000101220     c                   end
138100101220      *
138200101220     c                   endsr
138300101221     C*---------------------------------------------------------------*
138400101213**  err
138500101214Distinta non accessibile. Chiusa o Annullata                       01
138600101214Distinta non accessibile. Non usa il PDA.                          02
138700101214Data dist.anteriore o super.alla data odierna dei gg.ammessi       03
138800101221Linea arrivo spedizione non in gestione.                           04
138900101221Spedizione Inesistente. Non trovata in Arrivo.                     05
139000101213Distinta non accessibile. Allocata da altro lavoro.                06
139100101217Aggiornamento Distinta NON eseguito correttamente.                 07
139200101222Sped. impegnata da altro lavoro non è possibile Proseguire.        08
139300110808
139400101214Data distinta superiore alla data odierna dei gg.ammessi           10
139500101222La Distinta non è la stessa presente sulla Bolla                   11
139600101215Aggiornamento fallito. Bolla utilizzata da un altro lavoro.        12
139700101222La Bolla è già assegnata ad una altra Distinta                     13
139800110325Cod.Filiale non in forma numerica o mancante.                      14
139900110301Riferimenti distinta errati o mancanti                             15
140000110301Riferimenti spedizione errati o mancanti                           16
140100110301                                                                   17
140200110301                                                                   18
140300101222La Spedizione non è in distinta. Non è possibile toglierla.        19
140400110104Aggiornamento fallito. Bolla su altra distinta.                    20
140500160316La spedizione deve essere inserita in una distinta "fittizia"      21
