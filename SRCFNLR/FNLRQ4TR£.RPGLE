000100110104     h dftactgrp(*yes)
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
003700101213     FFNLR98D   CF   E             WORKSTN
003800101223     F*--------------------------------------------------------------*
003900021204     Ffiapd01L  IF   E           K DISK
004000101214     Ftntbe01L  IF   E           K DISK
004100101223      *----
004200101217     FFNARB01L  UF   E           K DISK    commit    infds(arbds)
004300101220     FFiarg01L  UF a E           K DISK    commit    infds(argds)
004400030730     FTABEL00F  iF   E           K DISK
004500030730     Ffnanm01l  iF   E           K DISK
004600090605     Ffnlbl01l  iF   E           K DISK
004700030730     FFNEVB04L  UF A E           K DISK    commit
004800070316     Fazorg01l  IF   E           K DISK
004900101223      *----
005000101223     Ffidst09L  IF   E           K DISK
005100940321      *---------------------------------------------------------------
005200030730     d* schiere
005300030730      *---------------------------------------------------------------
005400101221     D ERR             S             66    DIM(50) CTDATA PERRCD(1)             MSG ERRORE
005500101213      *---------------------------------------------------------------
005600030730     d* ds esterne
005700030730      *---------------------------------------------------------------
005800030730     D KPJBA         E DS
005900101217     D KPJBuS          s                   like(KPJBu)
006000030730     D ddatiute      e ds
006100030730     D azuteds       e ds                  extname(AZUTE00F)
006200110104     D fidstds       e ds                  extname(FIDST00F)
006300110105     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
006400110105     d fnarbds       e ds                  extname(fnarb00F)
006500110107     d fiargds$      e ds                  extname(fiarg00F) prefix($)
006600110107     d fiargds       e ds                  extname(fiarg00F)
006700030730     D tibs34ds      E DS                  inz
006800101221     D* DS PER - Bolle valide in consegna
006900070517     D fidg37ds      E DS
007000101220      * msg allocazione  inviato all'utente
007100101216     D trul82ds      E DS
007200080416     D ddstflr       E DS
007300100202     D dargflo       E DS
007400080416     c* elimina dati a pda
007500080416     D fidg42ds      E DS
007600080416     D fidg44ds      E DS
007700110105     D* DS PER - aggiornamento di ARB
007800110105     D fidg23ds      E DS
007900101214     D DDGD          E DS
008000100514     D DS7r          E DS
008100070316     D og146         E DS                  inz
008200080416     D og148         E DS                  inz
008300101222      * DS per richiamo pgm fnlr99r x gestione ORM in particolarità RC
008400030826     D fnlr99ds      E DS
008500031125      * DS per richiamo da fnlr50r x togliere bolla da distinta
008600031125     D fnlr98ds      E DS
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
012700030730      * DS X note eventi
012800001229     D DSNTEV          DS
012900001229     D  DISTEV                 1      7  0
013000001229     D  PADREV                 8     14  0
013100030730     d* num.relativo di rec. x file anomalie
013200990519     D FNANM1          DS
013300960415     D  ANMNR1               397    400B 0
013400030618     c* ds per sottocampare il barcode della spedizione
013500030618     D ds_pis          DS
013600030618     D  ds_pisaas                     2  0
013700030618     D  ds_pislnp                     3  0
013800030618     D  ds_pisnrs                     2  0
013900030618     D  ds_pisnsp                     7  0
014000030623     D  ds_chkdgt                     1  0
014100030730     d*---------------------------------------------------------------*
014200030730     d* campi di comodo                                               *
014300030730     d*---------------------------------------------------------------*
014400030730     d savchk          s                   like(o28ckd)
014500030730     d barcode         s             14
014600030730     d dtaeur          s               d   datfmt(*eur)
014700030730     d dtaiso          s               d
014800101214     d dtaiso_oggi     s               d   datfmt(*iso)
014900101214     d dtaiso_dst      s               d   datfmt(*iso)
015000101213      *-----
015100101213     D Found_Dist      S              1    INZ('N')
015200101213     D Error_Dist      S              1    INZ('N')
015300101213     D Rec_allocato    S              1    INZ('N')
015400101222     D Forza_aggiornamento...
015500101222     D                 s              1    INZ('N')
015600101223     D ALLOCA_Distinta...
015700101223     D                 s              1
015800110104     D ARG_Allocato...
015900110104     D                 s              1
016000101213     D*-------------
016100101213     d dstds           ds
016200101213     d  dst_nrr              397    400b 0
016300101217      *
016400101217     d argds           ds
016500101217     d  arg_nrr              397    400b 0
016600101217      *
016700101217     d arbds           ds
016800101217     d  arb_nrr              397    400b 0
016900101217      *
017000101222     C*-----------------------------------------------------*
017100101213      *
017200101214     D digits          c                   '0123456789'
017300101217     D Mettere         c                   'M'
017400101217     D Togliere        c                   ' '
017500101220     D SI              c                   'S'
017600101220     D NO              c                   'N'
017700911016      *---------------------------------------------------------------*
017800911016      * FLUSSO PRINCIPALE                                             *
017900911016      *---------------------------------------------------------------*
018000110104     C*
018100110104     C*   subito accende per usufruire del RETURN
018200110104     c                   seton                                        LR
018300110104     C*
018400110104     C* Se RICHIAMATO
018500110104     c                   if        Richiamato = SI
018600110104      * imposta i campi del video come se li avessero digitati
018700110104     c                   z-add     i98aas        v03ans
018800110104     c                   z-add     i98lnp        v03lnp
018900110104     c                   z-add     i98nsp        v03nsp
019000110104     c                   z-add     i98nrs        v03nrs
019100110104     c                   setoff                                       73
019200110104      **
019300110104      **  parametri obbligatori  FILIALE di GESTIONE
019400110104     c                   if        i98fgs = 0
019500110104     c                   eval      O98msgERR = ERR(14)
019600110104     c                   return
019700110104     c                   end
019800110104      *
019900110104      **  parametri obbligatori  DISTINTA SU CUI ESEGUIRE togli o metti
020000110104     c                   if        i98ndc = 0
020100110104     c                   eval      O98msgERR = ERR(15)
020200110104     c                   return
020300110104     c                   end
020400110104     c                   end
020500110104     C*
020600030730     C                   DO        *HIVAL
020700101222      * -----------
020800101216     C* VIDEO
020900101220      *--------  BYPASSATO se richiamato in OCCULTO
021000101220      **              --------------------------
021100101221     c                   if        Visual_Display = SI
021200101217      *--------
021300030730     c* accendo o spengo l'indicatore 73 che mi serve per il posizionamento
021400030730     c* 73 in caso di errore si posiziona sul barcode altrimenti sul numero
021500030730     c* della spedizione
021600030730     c                   if        *in90
021700030730     c                   if        (v03pis <> ' ' and v03nsp <> 0) or
021800101213     c                             (v03pis  = ' ' and v03nsp  = 0)
021900080605     c     combts        comp      'L'                                    73
022000030730     c                   end
022100101213     c                   if        (v03pis <> ' ' and v03nsp  = 0) or
022200101213     c                             (v03pis  = ' ' and v03nsp <> 0)
022300030730     c     v03pis        comp      ' '                                7373
022400030730     c                   end
022500101213      *
022600030730     c                   setoff                                       90
022700030730     C                   write     LR98D01
022800030730     c                   seton                                        90
022900030730     c                   end
023000030730     c*
023100101220     c                   If        Richiamato = SI
023200101222      * imposta i campi del video come se li avessero digitati
023300101222     c                   z-add     i98aas        v03ans
023400101222     c                   z-add     i98lnp        v03lnp
023500101222     c                   z-add     i98nsp        v03nsp
023600101222     c                   z-add     i98nrs        v03nrs
023700031125     c                   setoff                                       73
023800031125     c                   endif
023900101213     c*
024000101213      *                *-------------------*
024100030730     C                   EXFMT     LR98D01
024200101213      *                *-------------------*
024300030730     c* fine lavoro
024400941107     C     *INKC         IFEQ      '1'
024500030730     C                   LEAVE
024600941107     C                   END
024700101213      *--------
024800101220      ** IN OCCULTO SONO GIA' IMPOSTATI I CAMPI DELLA SPEDIZIONE
024900101213     c                   endif
025000101220      *--------      --------------------------------
025100030730     c* controlli
025200101220     c                   exsr      srctr
025300101223      *
025400101222      *  se ci sono errori  ed è richiamato deve uscire direttamente dal ciclo
025500101222     c   90              if        Richiamato = SI
025600101222     c                   leave
025700101220     c                   end
025800101221      * altrimenti per errori deve riproporre il video
025900101213     c   90              iter
026000101223      *
026100030730     c*aggiorna
026200101221     c                   if         Visual_display = SI and *inKF
026300101222     c                                 or
026400101222     c                              Forza_aggiornamento = SI
026500101222     c                                 or
026600101222     c                              Visual_Display = NO
026700101222     c                              and Interrogazione = NO
026800101221     c                              and Richiamato = SI
026900101213     c*
027000101213      *                *-------------------*
027100030730     c                   exsr      sraggio
027200101213      *                *-------------------*
027300101222     c*
027400101222     c*  per errori in aggiornamento
027500101222     c   90              if        Richiamato = SI
027600101222     c                   leave
027700101221     c                   end
027800030730     c   90              iter
027900101222      *
028000101221      *  Senza errori, se richiamato,
028100101222      *  esce dal ciclo a questo punto confermando il corretto aggiornamento.
028200101220     c                   If        Richiamato = SI
028300031125     c                   move      '0'           o98esito
028400031125     c                   leave
028500031125     c                   endif
028600101222      **
028700101222      * se è arrivato qui è in interattivo da menù quindi ha aggiornato
028800101221     c* e re-inizializza  il video per riemetterlo
028900101213      *                *-------------------*
029000030730     c                   exsr      srinz
029100101213      *                *-------------------*
029200030730     c                   end
029300101213      *
029400030730     c                   enddo
029500101222      * -----------
029600101213      *
029700101220     c                   If        Richiamato = SI
029800031125     c                   movel(p)  fnlr98ds      kpjbu
029900031125     c                   endif
030000101222     C*-----------------------------------------------------*
030100101222     C*    Operazioni iniziali
030200101222     C*-----------------------------------------------------*
030300101222     C     *INZSR        BEGSR
030400101222     C*
030500101222     C     *ENTRY        PLIST
030600101222     C                   PARM                    KPJBA
030700101222     C*
030800101222      * se richiamato da preparazione FNLR50R riceve i dati della spedizione
030900101222     c                   clear                   fnlr98ds
031000101222     c                   if        kpjbu <> *blank
031100101222     c                   movel     kpjbu         fnlr98ds
031200110104     c                   movel     kpjbu         kpjbus
031300101222     c                   endif
031400101222     C*
031500101222      *** inizializza campi di ritorno                                                ***
031600101222     c                   move      '1'           o98esito
031700101222     c                   move      *blank        O98MSGERR
031800101222     c                   move      NO            Richiamato        1
031900101222     c                   move      SI            Visual_Display    1
032000101222     c                   move      SI            Controlla_Dist    1
032100101222     c                   move      NO            Interrogazione    1
032200101223     c                   eval      ALLOCA_Distinta = SI
032300101222     c                   move      *blank        Modalita          1
032400101222      ***                                                                             ***
032500101222     C* Inverto la data del giorno
032600101222     C                   TIME                    WHHDAT           14 0
032700101222     C                   MOVEL     WHHDAT        HHMMss            6 0          *ORA/MINUTI/sec
032800101222     C                   MOVE      WHHDAT        G02DAT
032900101222     C                   MOVE      *BLANKS       G02ERR
033000101222     C                   CALL      'XSRDA8'
033100101222     C                   PARM                    WLBDA8
033200101222     C                   Z-ADD     G02INV        DATEU             8 0
033300101222     C*
033400101222     c* adesso non si deve più testare l'azione poichè tramite un parametro
033500101222     c*  si conosce se il programma è chiamato in interrogazione.
033600101222     c*****kcdaz         comp      'LR9A'                                 37
033700101222      *
033800101222     c                   setoff                                       37
033900101222      *  se richiamato come interrogazione
034000101222     c                   if        i98interr = SI
034100101222     c                   eval      Interrogazione = SI
034200101222     c                   seton                                        37
034300101222     c                   end
034400101222      *
034500101222      *  controlli specifici sulla Distinta da Bypassare poichè fatti dal chiamante
034600101222     c                   if        I98CTLDST = NO
034700101222     c                   eval      Controlla_Dist = NO
034800101222     c                   end
034900101223      *
035000101223      *** allocazione Distinta
035100101223     c                   if        I98ALCDST = NO
035200101223     c                   eval      ALLOCA_Distinta = NO
035300101223     c                   end
035400101222      *
035500101222      *  se richiamato deve mostrare il video
035600101222     c                   if        i98video  = NO
035700101222     c                   eval      Visual_Display = NO
035800101222     c                   end
035900101222      *
036000101222     c*  modalità di utilizzo del programma (Togliere o Mettere)
036100101222     c                   eval      modalita = i98delins
036200101222      *
036300101222     c*** Se Richiamato i campi della spedizione ci devono essere
036400101222     c                   if        i98aas > 0  or
036500101222     c                             i98lnp > 0  or
036600101222     c                             i98nsp > 0  or
036700101222     c                             i98video = SI
036800101222     c                   move      SI            Richiamato
036900101222     c                   end
037000101222     c*
037100101222     c     *dtaara       define    §azute        azuteds
037200101222     c     *dtaara       define    §datiute      ddatiute
037300101222     C                   in(E)     *dtaara
037400101222     c*
037500101222     C                   IF        %Error  or  RSUT = *blanks
037600101222     C                   call      'TIBS34R'
037700101222     C                   parm                    Tibs34Ds
037800101222     C                   in        *dtaara
037900101222     c                   ENDIF
038000101222     c*
038100101222     c* controllo se p.o. partito non può + usare la funzione ma deve
038200101222     c* agire dal pgm di assegnaz./disassegnaz.
038300101222     C                   move      *date         dtaeur
038400101222     C                   move      dtaeur        dtaiso
038500101222     c                   move      dtaiso        oggin             8 0
038600101222     c                   move      oggin         oggi              8
038700101222     C     dutpou        CHAIN     AZORG01l
038800101222     c*
038900101222    1C                   IF        %found(azorg01l)
039000101222     C                   MOVEL     ORGde6        og146
039100101222     c                   movel     orgde8        og148
039200101222     c                   end
039300101222     c*
039400101222      * se c'è una data in organigramma
039500101222     c                   move      §ogdda        data_org          8 0
039600101222     C                   MOVE      *BLANK        test_Parallelo    1
039700101222     c                   if        §ogdda <> ' '
039800101222      * se siamo in fase di Parallelo prima di essere partiti
039900101222     c                   if        data_org = 20391231 or dateu < data_org
040000101222     C                   eval      test_Parallelo = 'S'
040100101222     c                   end
040200101222     c                   end
040300101222     c*
040400101222     C* Aggancio la £6 e recupero le linee a loro associate
040500101222     c                   clear                   trul06ds
040600101222     c                   eval      d06cod = '£6'
040700101222     c                   movel     dutpou        d06key
040800101222     c                   movel     trul06ds      kpjbu
040900101222     c                   call      'TRUL06R'
041000101222     c                   parm                    kpjba
041100101222     c                   movel     kpjbu         trul06ds
041200110104     c                   eval      kpjbu = kpjbus
041300101222      *
041400101222     c* accendo o spengo l'indicatore 36 per gestire in seguito il campo
041500101222     c* dove andrà il barcode della spedizione
041600101222     C                   MOVEL     'L'           comBTS
041700101222     c     combts        comp      'L'                                    36
041800101222     c*
041900101222     c                   if        combts = 'L'
042000101222     c                   eval      v03f2 = 'F2=Manuale'
042100101222     c                   else
042200101222     c                   eval      v03f2 = 'F2=Pistola'
042300101222     c                   end
042400101222      *---------------------------*
042500101222      * CHIAVI                                                        *
042600101222      *---------------------------*
042700110107     C     K_autista     KLIST
042800101222     C                   KFLD                    apdtip
042900101222     C                   KFLD                    v03pdr
043000101222     c                   movel     'A'           apdtip
043100101222     C     KTAB          KLIST
043200101222     C                   KFLD                    tblkut
043300101222     C                   KFLD                    TBLCOD
043400101222     C                   KFLD                    TBLKEY
043500110107     C     Bolla_ARB     KLIST
043600101222     C                   KFLD                    ARBAAS
043700101222     C                   KFLD                    ARBLNP
043800101222     C                   KFLD                    ARBNRS
043900101222     C                   KFLD                    ARBNSP
044000110107     C     K_Distinta    KLIST
044100101222     C                   KFLD                    dstfgs
044200101222     C                   KFLD                    dstnpg
044300101222     C                   KFLD                    dstnfv
044400110107     C     Bolla_Prec    KLIST
044500101222     C                   KFLD                    LBLAAP
044600101222     C                   KFLD                    LBLLPP
044700101222     C                   KFLD                    LBLNRP
044800101222     C                   KFLD                    LBLNSP
044900101222     c                   eval      dstnpg = 4
045000110107     C     Evb_Bolla     KLIST
045100101222     C                   KFLD                    KAAS
045200101222     C                   KFLD                    KLNP
045300101222     C                   KFLD                    KNRS
045400101222     C                   KFLD                    KNSP
045500101222     C                   KFLD                    KCEV
045600101222     C*
045700110107     C     Anm_Bolla     KLIST
045800101222     C                   KFLD                    WAA4
045900101222     C                   KFLD                    ARBLNP
046000101222     C                   KFLD                    ARBNRS
046100101222     C                   KFLD                    ARBNSP
046200101222     C                   KFLD                    §§§CAA            3 0
046300101222     C*
046400101222     C     *LIKE         DEFINE    ARBIFP        KFGS
046500101222     C     *LIKE         DEFINE    ARBNDC        KNDC
046600101222     C     *LIKE         DEFINE    ARBDDC        KDDC
046700101222     C     *LIKE         DEFINE    EVBAAS        KAAS
046800101222     C     *LIKE         DEFINE    EVBLNP        KLNP
046900101222     C     *LIKE         DEFINE    EVBNRS        KNRS
047000101222     C     *LIKE         DEFINE    EVBNSP        KNSP
047100101222     C     *LIKE         DEFINE    EVBCEV        KCEV
047200101222     C     *LIKE         DEFINE    TBECOD        KCOD1
047300101222     C     *LIKE         DEFINE    TBEKE1        KKEY1
047400101222     C*
047500101222     C                   MOVEL     'FNLR98R'     VIDPGM
047600101222     C                   MOVE      *year         V03ANS
047700101222     C*
047800101222     C                   ENDSR
047900101220     c**********************************************************************
048000101220     C     srctr         BEGSR
048100101220     c**********************************************************************
048200101220     C                   SETOFF                                       90
048300101222     c                   eval       Forza_aggiornamento = 'N'
048400101220      *
048500101220      *  Campi Inseriti a Video : Controllo di inserimento e correttezza
048600101221     c                   IF        Visual_Display = SI
048700101220      ****
048800101222     c* se il campo v03pis è impostato valorizzo i campi della spedizione
048900030620     c* solo se non sono stati impostati a mano o al giro di barcode che poi
049000030620     c* ha dato l'errore
049100030618     c                   clear                   ds_pis
049200030730     c                   if        v03pis <> ' '
049300030730     c                   movel     v03pis        ds_pis
049400030623     c                   if        v03nsp = 0 and
049500030620     c                             v03lnp = 0 and v03nrs = 0
049600030730     C                   MOVE      *year         V03ANS
049700030730     c                   move      ds_pisaas     v03ans
049800030618     c                   move      ds_pislnp     v03lnp
049900030618     c                   move      ds_pisnrs     v03nrs
050000030618     c                   move      ds_pisnsp     v03nsp
050100030623     c* controllo il check digits
050200030623     c                   exsr      srchkdgt
050300030730     c                   if        *in10
050400101220     c                   leaveSR
050500030618     c                   end
050600030623     c                   else
050700030730     c                   move      v03ans        com02             2 0
050800030623     c                   if        com02<>ds_pisaas or v03lnp<>ds_pislnp or
050900030623     c                             v03nrs<>ds_pisnrs or v03nsp<>ds_pisnsp
051000030730     c                   seton                                        1190
051100101220     c                   leaveSR
051200030623     c                   end
051300030623     c                   end
051400030623     c                   end
051500101221      *
051600030730     C* F2-Inserimento spedizione da manuale a pistola o viceversa
051700030730     c* serve per il posizionamento sulla spedizione o sul barcode
051800030730     C                   if        *inkb
051900080605     c                   if        combts = 'L'
052000080605     c                   movel     *blanks       combts
052100030703     c                   eval      v03f2 = 'F2=Pistola'
052200030703     c                   seton                                        90
052300030703     c                   setoff                                       7336
052400101220     c                   leaveSR
052500030703     c                   else
052600080605     c                   movel     'L'           combts
052700030703     c                   eval      v03f2 = 'F2=Manuale'
052800030703     c                   seton                                        907336
052900101220     c                   leaveSR
053000030703     c                   end
053100030730     C                   END
053200101221      *
053300030730     C*F7-INTERROGAZIONE BOLLE ARRIVI
053400030730     C                   if        *inkg
053500941109     C                   MOVEL     '2'           P36FLG
053600030730     C                   Z-ADD     V03ANS        P36AAS
053700941108     C                   Z-ADD     V03LNP        P36LNP
053800941108     C                   Z-ADD     V03NSP        P36NSP
053900941108     C                   Z-ADD     V03NRS        P36NRS
054000941109     C                   MOVEL     ' '           P36F03
054100941109     C     P36NSP        IFGT      0
054200941109     C                   MOVEL     '1'           P36FLG
054300941109     C                   END
054400941108     C                   MOVEL     DSLR36        KPJBU
054500941108     C                   CALL      'FNLR36R'
054600911128     C                   PARM                    KPJBA
054700941109     C                   MOVEL     KPJBU         DSLR36
054800941109     C     P36F03        IFNE      '1'
054900941215     C     V03NSP        ANDEQ     0
055000030730     C                   Z-ADD     P36AAS        V03ANS
055100941109     C                   Z-ADD     P36LNP        V03LNP
055200941109     C                   Z-ADD     P36NRS        V03NRS
055300941109     C                   Z-ADD     P36NSP        V03NSP
055400941109     C                   END
055500911128     C                   SETON                                        90
055600101220     c                   leaveSR
055700911128     C                   END
055800101220      ****
055900101220     c                   endIF
056000101221      ****
056100101220      * ------------------------------------
056200101220      ** Dopo il Video         i campi sono stati impostati
056300101220      **               oppure  arrivano direttamente dai Parametri se richiamato
056400101220      * ------------------------------------
056500101220      *   Controlli GENERALI SULLA BOLLA
056600101220      * ------------------------------------
056700101222     C* Controllo se la spedizione è esistente o valida su video
056800101220     C     V03ANS        IFEQ      *ZEROS
056900101220     C                   SETON                                        0190
057000101221     c                   eval      O98msgERR = ERR(16)
057100101220     c                   leaveSR
057200101220     C                   END
057300101220     C     V03LNP        IFEQ      *ZEROS
057400101220     C                   SETON                                        0290
057500101221     c                   eval      O98msgERR = ERR(17)
057600101220     c                   leaveSR
057700101220     C                   END
057800101220     C     V03NSP        IFEQ      *ZEROS
057900101220     C                   SETON                                        0390
058000101221     c                   eval      O98msgERR = ERR(18)
058100101220     c                   leaveSR
058200101220     C                   END
058300101220      **
058400101220     C                   Z-ADD     V03ANS        ARBAAS
058500101220     C                   Z-ADD     V03LNP        ARBLNP
058600101220     C                   Z-ADD     V03NRS        ARBNRS
058700101220     C                   Z-ADD     V03NSP        ARBNSP
058800101223      *------->>>
058900101220     C* AGGANCIO FNARB
059000110107     C     Bolla_ARB     CHAIN(n)  FNARB01L
059100101220      *
059200101220      *   NON TROVATA LA BOLLA
059300101220     c                   if        not %found(fnarb01l)
059400101220     c                   seton                                        0590
059500101220     c                   eval      O98msgERR = ERR(05)
059600101220      **** BOLLA INESISTENTE
059700101220     c                   leaveSR
059800101220     c                   end
059900101220     C*-------------
060000101220      ** TROVATA LA BOLLA, Controllo se linea di arrivo abilitata
060100101220     C     ARBLNA        LOOKUP    LINT                                   70
060200101220     c                   if        not *in70
060300101220     C                   SETON                                        0490
060400101220     c                   eval      O98msgERR = ERR(04)
060500101220     c                   leaveSR
060600101220     c                   end
060700101223      *
060800101220      ** --------------------------**
060900101220      **  DECODIFICHE da ARB
061000101220      ** --------------------------**
061100110105     c                   z-add     i98npg        dstnpg
061200101222      * se su ARB c'è la distinta
061300110104     c                   if        Modalita = Togliere
061400101220     c                   z-add     arbifp        v03ifp
061500101220     c                   z-add     arbndc        v03ndc
061600110104     c                   z-add     arbifp        dstfgs
061700110104     c                   z-add     arbndc        dstnfv
061800110105      *
061900110104     c                   elseIf    Modalita = Mettere
062000110104      * se metto su ARB NON c'è la distinta
062100110104     c                   z-add     i98fgs        v03ifp
062200110105     c                   z-add     i98fgs        dstfgs
062300110104     c                   z-add     i98ndc        v03ndc
062400110105     c                   z-add     i98ndc        dstnfv
062500110104     c                   end
062600101220      *   Data Distinta
062700101220     c                   if        arbddc <> 0
062800101220     c                   move      arbddc        dtaiso
062900101220     c                   move      dtaiso        dtaeur
063000101220     c                   move      dtaeur        v03ddc
063100101220     c                   else
063200101220     c                   z-add     0             v03ddc
063300101220     c                   end
063400110105      *
063500110105      *  in eccezione:  se non fosse allineato li prende dai parametri
063600110105     c                   if        v03ifp = 0
063700110105     c                   z-add     i98fgs        v03ifp
063800110105     c                   z-add     i98fgs        dstfgs
063900110105     c                   end
064000110105     c                   if        v03ndc = 0
064100110105     c                   z-add     i98ndc        v03ndc
064200110105     c                   z-add     i98ndc        dstnfv
064300110105     c                   end
064400101220      *
064500101220     C* decodifica mattina/pomeriggio
064600101220     c                   clear                   v03fpp
064700110107     C     k_Distinta    CHAIN     fidst09L
064800101220      *
064900101220     c                   if        %found(fidst09l)
065000110104      *
065100110104     c                   if        v03ddc  = 0
065200110104     c                   move      dstdfv        dtaiso
065300110104     c                   move      dtaiso        dtaeur
065400110104     c                   move      dtaeur        v03ddc
065500110104     c                   end
065600101220      *
065700110104     c                   eval      ddstflr = dstflr
065800101220     c                   if        dstfpp = 'M'
065900101220     C                   eval      v03fpp = 'Mattino'
066000101220     c                   elseIf    dstfpp = 'P'
066100101220     C                   eval      v03fpp = 'Pomeriggio'
066200101220     c                   end
066300110104      *
066400110104      ** --------------------------**
066500110104     C* decodifica padroncino    Autista
066600110104      ** --------------------------**
066700110104     c                   move      arbpdc        v03pdr
066800110104     c                   if        v03pdr  = 0
066900110104     c                   move      dstpdr        v03pdr
067000110104     c                   end
067100110104      *
067200110104     c                   clear                   v03rsc
067300110107     C     K_autista     CHAIN     fiapd01L
067400110104     c                   if        %found(fiapd01l)
067500110104     C                   MOVEl(p)  APDRSC        V03rsc
067600110104     c                   end
067700101220      *
067800101220      * ------------------------------
067900101220      *    Controlli specifici per TOGLI o METTI
068000101220      * ------------------------------
068100101220      **** SPECIFICI per TOGLIERE DA UNA Distinta
068200101221     c                   if        Modalita = Togliere
068300101220      *                *-------------------*
068400101220     c                   exsr      srctr_Togli
068500101220      *                *-------------------*
068600101220     c   90              leaveSR
068700101220      ****
068800101220      **** SPECIFICI per METTERE  SU UNA Distinta
068900101221     c                   elseIf    Modalita = Mettere
069000101220      ****
069100101220      *                *-------------------*
069200101220     c                   exsr      srctr_Metti
069300101220      *                *-------------------*
069400101220     c   90              leaveSR
069500101220     c                   end
069600101220      *
069700101220     c* Controlli aggiunti da eseguire su richiesta in OCCULTO
069800101223     c                   if        Interrogazione = NO
069900101220      *                           *----------*
070000101220     c                   exsr      srctl_dist
070100101220      *                           *----------*
070200101220     c   90              leaveSR
070300101220     c                   end
070400101220      *
070500101220     c                   endIF
070600101220      * ------------------------------
070700101220      *   Controlli x campi impostati a video
070800101222     c                   IF        Visual_Display = SI
070900101220      *
071000101222     C* se non c'è l'errore ind.11 (scelto sia spedizione che barcode)
071100101220     c* azzero la spedizione o il barcode a seconda di come sto inserendo
071200101220     c* i riferimenti della spedizione
071300101220     c                   if        not *in11
071400101220     c                   if        combts = 'L' and v03nsp <> 0 and
071500101220     c                             v03pis <> ' '
071600101220     C                   MOVE      *year         V03ANS
071700101220     C                   Z-ADD     *ZEROS        V03LNP
071800101220     C                   Z-ADD     *ZEROS        V03NRS
071900101220     C                   Z-ADD     *ZEROS        V03NSP
072000101220     c                   end
072100101220     c                   if        combts <> 'L' and v03nsp <> 0 and
072200101220     c                             v03pis <> ' '
072300101220     C                   clear                   V03pis
072400101220     c                   end
072500101220     c                   end
072600101220      *
072700101220     c                   endIF
072800101220      *
072900101220     c                   endsr
073000101220     c**********************************************************************
073100101220     C     srctr_Togli   BEGSR
073200101220     c**********************************************************************
073300101220      *  SPECIFICI per TOGLIERE una spedizione da una DISTINTA
073400101220      *---------------
073500101220      * Per Togliere:
073600101220     C* controllo la distinta che deve essere presente sulla Bolla
073700101220     c                   if        arbndc = 0
073800030730     c                   z-add     0             v03ifp
073900030730     c                   z-add     0             v03ndc
074000030730     c                   z-add     0             v03ddc
074100030730     c                   z-add     0             v03pdr
074200030730     c                   clear                   v03rsc
074300080415     c                   clear                   v03fpp
074400030730     c                   seton                                        0690
074500101222      *
074600101222      * se non c'è NON SI PUO' TOGLIERE .........
074700101221     c                   eval      O98msgERR = ERR(19)
074800101220     c                   leaveSR
074900101217     c                   end
075000101221      * ---------
075100101220      **  se richiamato, PER Togliere
075200101220      **       La distinta passata deve corrispondere a quella della bolla
075300101220     c                   If        Richiamato = SI
075400101220     c                   z-add     0             parmNDC           7 0
075500110103     c                   z-add     I98NDC        parmNDC
075600101220     c                   if          parmNDC <> arbNDC
075700101220     c                   eval      O98msgERR = ERR(11)
075800101220     c                   leaveSR
075900101220     c                   end
076000101220      *
076100101220     c                   endIf
076200101220      *---------
076300911017     C                   ENDSR
076400101220     c**********************************************************************
076500101220     C     srctr_Metti   BEGSR
076600101220     c**********************************************************************
076700101223      **   deve controllare che la Bolla non sia già legata
076800101223      **    ad un'altra distinta
076900101223     c                   if          arbNDC <> 0
077000101220     c                   eval      O98msgERR = ERR(13)
077100101220     c                   leaveSR
077200101220     c                   endIf
077300101220      *
077400110107     C* passa se c'è anche l'ARG
077500110107     c                   clear                   fiargds$
077600110107     c     Bolla_ARB     chain(n)  fiarg01l
077700110107     c                   if        %Found(Fiarg01L)
077800110107     c                   eval      fiargds$ = fiargds
077900110107     c                   end
078000110107     C*
078100101223     C*   esternizzata Routine di controlli
078200101223     C*      sulla Consegnabilità della Bolla
078300101223     c                   clear                   Fnlrq4DS
078400101223     c                   z-add     v03ans        LRQ4aasI
078500101223     c                   z-add     v03lnp        LRQ4lnpI
078600101223     c                   z-add     v03nrs        LRQ4nrsI
078700101223     c                   z-add     v03nsp        LRQ4nspI
078800101223     c                   z-add     dstnfv        LRQ4ndcI
078900101223     c                   z-add     dstdfv        LRQ4ddcI
079000110111     c                   move      'S'           LRQ4conI                       consegnabilità
079100110105     c                   eval      fnarbds$ = fnarbds
079200101223     c                   eval      kpjbu = Fnlrq4DS
079300101223     c                   call      'FNLRQ4R'
079400101223     c                   parm                    kpjba
079500110105     c                   parm                    fnarbds$
079600110107     c                   parm                    fiargds$
079700101223     c                   eval      Fnlrq4DS = kpjbu
079800110104     c                   eval      kpjbu = kpjbus
079900101223      *
080000101223      *   Dopo i controlli della bolla se Fermo Deposito
080100101223      *    deve forzare ed andare avanti
080200101223      *    ....deve eliminare la segnalazione di errore
080300101223      *
080400101223     c                   if        LRQ4ERRO <> *blank
080500101223      * imposta l'errore
080600101223     c                   eval      O98msgERR = LRQ4MSGO
080700101223     c                   seton                                        90
080800110104      ********
080900101223      *  Chiede eventualmente la forzatura
081000101223      *      Forza x il tipo di errore  'F' Forzabile l'Avanzamento o meno.....
081100110104     c********           if        LRQ4ERRO  = 'F'
081200110104     c********           eval      err001   = LRQ4MSGO
081300110104     c********           eval      err002   = 'Forzare comunque l''avanzamento-
081400110104     c********                                 con F6'
081500110104     c*****video_err     tag
081600110104     c********           exfmt     LR98WER
081700110104     c**nKF***
081800110104     c***annKL              goto      video_err
081900110104      ********
082000101223      * se Forza l'avanzamento elimina segnalazione di errore
082100110104     c********           if        *inKF
082200110104     c********           clear                   O98msgERR
082300110104     c********           eval      *in90 = *off
082400110104     c********           end
082500110104     c********           end
082600110104      ********
082700101223     c   90              leaveSR
082800101223     c                   end
082900101223     C*
083000101223     C                   ENDSR
083100080416     C*-----------------------------------------------------*
083200080416     C* pgm per forzare modifica distinta fase CUS          *
083300080416     C*-----------------------------------------------------*
083400101214     C     srctl_dist    BEGSR
083500101213      *
083600101223     c                   clear                   FnlrQ3ds
083700101223     c                   eval      LRQ3NPGI = dstnpg
083800101223     c                   eval      LRQ3FGSI = dstfgs
083900101223     c                   eval      LRQ3NDCI = dstnfv
084000101223     c                   eval      LRQ3ALCI = 'N'
084100101223     c                   if        ALLOCA_Distinta = SI
084200101223     c                   eval      LRQ3ALCI = 'S'
084300101223     c                   end
084400101223     C*
084500101223     C*   esternizzata Routine di controlli  sulla Bolla se consegnabile
084600101223     c                   eval      kpjbu = Fnlrq3DS
084700101223     c                   call      'FNLRQ3R'
084800101223     c                   parm                    kpjba
084900110104     c                   parm                    fidstds
085000101223     c                   eval      Fnlrq3DS = kpjbu
085100110104     c                   eval      Kpjbu = kpjbus
085200101223      *
085300101223     c                   if        LRQ3ERRO <> *blank
085400101223     c                   eval      O98msgERR = LRQ3MSGO
085500101223     c                   seton                                        90
085600101223     c                   leaveSR
085700101223     c                   end
085800101214      * ---------------
085900101214     C                   ENDSR
086000911018     C*-----------------------------------------------------*
086100030623     C* controllo check digits
086200911018     C*-----------------------------------------------------*
086300030623     C     srchkdgt      BEGSR
086400030623     c                   movel     ds_pis        barcode
086500030623     C                   clear                   trul28ds
086600030623     c                   eval      i28mod = 'BAR'
086700030623     c                   eval      i28cod = barcode
086800030623     c                   call      'TRUL28R1'
086900030623     c                   parm                    trul28ds
087000030623     c* se reperito barcode imposto i campi in stampa
087100030623     c                   if        o28err <> *blanks or  o28cod <> ds_pis
087200090904     c                   seton                                        1090
087300030623     c                   end
087400101223      **
087500030623     C                   ENDSR
087600030730     C*-----------------------------------------------------*
087700030730     C* sraggio - aggiorno FNARB, anomalie ed eventi        *
087800030730     C*-----------------------------------------------------*
087900030730     c     sraggio       begsr
088000110105     c                   move      'N'           esegue_ROLBK      1
088100101213      **
088200030730     C* AGGANCIO FNARB
088300110107     C     Bolla_ARB     CHAIN(e)  FNARB01L
088400101214      *
088500101214     c                   if        %error
088600101214      * Bolla non allocabile per l'aggiornamento
088700101214     c                   eval      O98msgERR = ERR(12)
088800101214     c                   seton                                          90
088900101214     c                   leavesr
089000101214     c                   end
089100101213      **
089200101220      **  Solo se richiamata x TOGLIERE:
089300101221     c                   if        Modalita = Togliere
089400101220      *
089500030730     C* aggiorno le eventuali anomalie sulla bolla
089600030730     C                   EXSR      ANOM
089700030730     C* tolgo l'evento MIC o creo il NIC
089800980309     C                   EXSR      ANNMIC
089900110107      **
090000110107     C                   Z-ADD     *ZEROS        ARBNSS
090100110107     C                   Z-ADD     *ZEROS        ARBIFP
090200110107     C                   Z-ADD     *ZEROS        ARBNDC
090300110107     C                   Z-ADD     *ZEROS        ARBDDC
090400110107     C                   Z-ADD     *ZEROS        ARBSTP
090500110107     C                   Z-ADD     *ZEROS        ARBNGD
090600110107     C                   Z-ADD     *ZEROS        ARBPDC
090700101222      *
090800101222     c                   end
090900101220      *
091000101222     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
091100101220     c* deve andare a togliere la distinta dall'orm
091200101220     c                   exsr      srorm
091300110107      **
091400101214     C                   UPDATE    FNARB000
091500101214      **
091600071002     c* aggiorna pesi/volumi/fermate assegnate x fidst00f
091700101222     c                   if        §ogdda <> ' '
091800101223     c                   exsr      srfiarg
091900110107     c   90              move      'S'           esegue_ROLBK
092000101215      **
092100101223      * solo se aggiornava ....
092200101223     c                   if        ALLOCA_Distinta = SI
092300071002     c                   exsr      srfidg37
092400110107     c   90              move      'S'           esegue_ROLBK
092500101223     c                   end
092600101215      **
092700071002     c                   end
092800101213      *
092900101222      * il commit se il programma è richiamato in cascata da altri
093000101222      *   può essere eseguito dall'esterno da altri programmi.
093100101213     c                   if        I98STRCMT = ' '
093200110105     c                   if          esegue_ROLBK = 'N'
093300110105     c                   COMMIT
093400110105     c                   else
093500110105     c                   ROLBK
093600110105     c                   end
093700101214     c                   end
093800101213      *
093900080416     c* richiamo il pgm per aggiornare i dati per PDA
094000101222     c                   if        §OGPDAcon <> ' ' and
094100080416     c                             (dstpda =  'C' or dstpda = 'E' or
094200101222     c                             §DSTTSTPDA= 'C' or
094300101222     c                             §DSTTSTPDA= 'E')
094400080506     c                   clear                   fidg42ds
094500080506     c                   eval      co42tla = 'A'
094600080416     c                   exsr      srpda
094700080416     c                   end
094800101220      *
094900030730     c                   endsr
095000070419     C*-----------------------------------------------------*
095100070517     C* aggiorna pesi/volumi/fermate assegnate x fidst00f   *
095200070419     C*-----------------------------------------------------*
095300070517     C     srfidg37      BEGSR
095400070419     c*
095500070517     c                   clear                   fidg37ds
095600101223     c                   eval      D37FGSI = dstfgs
095700101223     c                   eval      D37DFVI = dstdfv
095800101223     c                   eval      D37NFVI = dstnfv
095900070517     c                   eval      D37cmti = 'N'
096000070517     c                   eval      kpjbu = fidg37ds
096100070517     c                   call      'FIDG37R'
096200070517     c                   parm                    kpjba
096300070517     c                   eval      fidg37ds = kpjbu
096400110104     c                   eval      Kpjbu = kpjbus
096500101220      *
096600101214     c                   if        D37ERRO <> ' '
096700101215      * Distinta non allocabile per l'aggiornamento
096800101215     c                   eval      O98msgERR = ERR(06)
096900101214     c                   seton                                          90
097000101214     c                   leavesr
097100101214     c                   end
097200070419     c*
097300070419     C                   ENDSR
097400980309     C*-----------------------------------------------------*
097500980309     C* ANNMIC: Annullo evento di  "Messa in consegna"      *
097600980309     C*-----------------------------------------------------*
097700980309     C     ANNMIC        BEGSR
097800030730     C                   move      *date         dtaeur
097900030730     C                   move      dtaeur        dtaiso
098000090605     C* Controllo se ho scritto evento MIC
098100110107     C     Bolla_ARB     CHAIN     FNLBL01L                           87
098200090605     C*
098300090605     C     *IN87         IFEQ      '0'
098400090605     C     ARBLNA        ANDEQ     LBLLAP
098500110107     C     Bolla_Prec    CHAIN     FNARB01L                           30
098600090605     C     ARBCCA        ifeq      '2'
098700090605     C     ARBCCA        oreq      '6'
098800090605     C                   SETON                                        87
098900090605     C                   END
099000090827     c* manuale
099100090827     C                   Z-ADD     V03ANS        ARBAAS
099200090827     c                   if        v03nsp <> 0
099300090605     C                   Z-ADD     V03LNP        ARBLNP
099400090605     C                   Z-ADD     V03NRS        ARBNRS
099500090605     C                   Z-ADD     V03NSP        ARBNSP
099600090827     c* pistola
099700090827     c                   else
099800090827     c                   movel     v03pis        ds_pis
099900090827     c                   move      ds_pislnp     arblnp
100000090827     c                   move      ds_pisnrs     arbnrs
100100090827     c                   move      ds_pisnsp     arbnsp
100200090827     c                   end
100300110107     C     Bolla_ARB     CHAIN     FNARB01L                           30
100400090605     C                   END
100500090605     C*
100600090605     C     *IN87         IFEQ      '0'
100700090605     C     ARBLNA        ANDEQ     LBLLAP
100800090605     C                   Z-ADD     LBLAAP        KAAS
100900090605     C                   Z-ADD     LBLLPP        KLNP
101000090605     C                   Z-ADD     LBLNRP        KNRS
101100090605     C                   Z-ADD     LBLNSP        KNSP
101200090605     C                   ELSE
101300090605     C                   Z-ADD     ARBAAS        KAAS
101400090605     C                   Z-ADD     ARBLNP        KLNP
101500090605     C                   Z-ADD     ARBNRS        KNRS
101600090605     C                   Z-ADD     ARBNSP        KNSP
101700090605     C                   END
101800980309     C                   MOVEL     'MIC'         KCEV
101900110107     C     Evb_Bolla     SETGT     FNEVB04L
102000110107     C     Evb_Bolla     READPE    FNEVB04L                               33
102100980309     C     *IN33         IFEQ      '0'
102200101223     C     dstDFV        ANDEQ     EVBDEV
102300101222     C* Se MIC esiste e la data della distinta è la stessa
102400101222     C* dell'evento ... controllo se è già trasmesso
102500980311     C     EVBDTR        IFNE      0
102600990803     C                   MOVEL     EVBDEV        WDTMIC           12 0
102700980311     C                   MOVE      EVBHEV        WDTMIC
102800101222     C* se l'evento di messa in consegna è stato trasmesso
102900101222     C* controllo se esiste già un evento NIC tolto dalla
103000980311     C* consegna
103100980311     C                   MOVEL     'NIC'         KCEV
103200110107     C     Evb_Bolla     SETGT     FNEVB04L
103300110107     C     Evb_Bolla     READPE    FNEVB04L                               33
103400031021     C* SE HO TROVATO EVENTO NIC
103500030730     C     *IN33         IFEQ      '0'
103600101223     C     dstDFV        ANDEQ     EVBDEV
103700101222     C* Se non è stato trasmesso aggiorno la data e l'ora di
103800031021     C* di immissione evento con quella del giorno + altri dati
103900031021     C     EVBDTR        IFEQ      0
104000031021     C                   move      dtaiso        EVBDTV
104100031021     C                   TIME                    EVBORV
104200031021     C                   Z-ADD     dutpou        EVBFLE
104300031021     C                   Z-ADD     DUTCOU        EVBCDU
104400101223     C     dstFPP        IFEQ      'M'
104500031021     C                   MOVEL     0800          EVBHEV
104600031021     C                   ELSE
104700031021     C                   MOVEL     1200          EVBHEV
104800031021     C                   END
104900031021     C                   MOVEL     *BLANKS       EVBDTC
105000101214      *
105100031021     C                   UPDATE    FNEVB000
105200101214      *
105300101222     C* Se è stato trasmesso controllo se non ha data più
105400031021     C* alta del MIC
105500031021     C                   ELSE
105600031021     C                   MOVEL     EVBDEV        WDTNIC           12 0
105700031021     C                   MOVE      EVBHEV        WDTNIC
105800031021     C     WDTNIC        IFLE      WDTMIC
105900101222     C* Se la data non è più alta scrivo un nuovo NIC
106000031021     C                   CLEAR                   FNEVB000
106100031021     C                   MOVEL     'NIC'         EVBCEV
106200031021     C                   move      dtaiso        EVBDTV
106300031021     C                   TIME                    EVBORV
106400031021     C                   Z-ADD     dutpou        EVBFLE
106500031021     C                   Z-ADD     DUTCOU        EVBCDU
106600031021     C                   Z-ADD     KAAS          EVBAAS
106700031021     C                   Z-ADD     KLNP          EVBLNP
106800031021     C                   Z-ADD     KNRS          EVBNRS
106900031021     C                   Z-ADD     KNSP          EVBNSP
107000101223     C                   Z-ADD     dstDFV        EVBDEV
107100101223     C     dstFPP        IFEQ      'M'
107200031021     C                   MOVEL     0800          EVBHEV
107300031021     C                   ELSE
107400031021     C                   MOVEL     1200          EVBHEV
107500031021     C                   END
107600031021     C                   Z-ADD     ARBNDC        DISTEV
107700031021     C                   Z-ADD     ARBPDC        PADREV
107800031021     C                   MOVEL     DSNTEV        EVBNOT
107900031021     C                   MOVEL     *BLANKS       EVBDTC
108000101214      *
108100031021     C                   WRITE     FNEVB000
108200101214      *
108300031021     C                   END
108400031021     C*
108500031021     C                   END
108600031021     C* SE NON ESISTE EVENTO NIC avente la stessa data evento della
108700031021     C* distinta lo scrivo
108800030730     C                   ELSE
108900031021     C                   CLEAR                   FNEVB000
109000031021     C                   MOVEL     'NIC'         EVBCEV
109100031021     C                   move      dtaiso        EVBDTV
109200031021     C                   TIME                    EVBORV
109300031021     C                   Z-ADD     dutpou        EVBFLE
109400031021     C                   Z-ADD     DUTCOU        EVBCDU
109500031021     C                   Z-ADD     KAAS          EVBAAS
109600031021     C                   Z-ADD     KLNP          EVBLNP
109700031021     C                   Z-ADD     KNRS          EVBNRS
109800031021     C                   Z-ADD     KNSP          EVBNSP
109900101223     C                   Z-ADD     dstDFV        EVBDEV
110000101223     C     dstFPP        IFEQ      'M'
110100031021     C                   MOVEL     0800          EVBHEV
110200031021     C                   ELSE
110300031021     C                   MOVEL     1200          EVBHEV
110400031021     C                   END
110500031021     C                   Z-ADD     ARBNDC        DISTEV
110600031021     C                   Z-ADD     ARBPDC        PADREV
110700031021     C                   MOVEL     DSNTEV        EVBNOT
110800031021     C                   MOVEL     *BLANKS       EVBDTC
110900101214      *
111000031021     C                   WRITE     FNEVB000
111100980311     C*
111200030730     C                   END
111300101222     C* Se MIC non è stato trasmesso annullo fisicamente record
111400980311     C                   ELSE
111500980309     C* annullo fisicamente l'evento se da trasmettere
111600101214      *
111700990803     C  N33              DELETE    FNEVB04L
111800101214      *
111900030730     C                   END
112000030730     C                   END
112100980309     C*
112200980309     C                   ENDSR
112300930623     C*-----------------------------------------------------*
112400930623     C* GESTIONE ANOMALIE DELLA BOLLA                       *
112500930623     C*-----------------------------------------------------*
112600930623     C     ANOM          BEGSR
112700930623     C*
112800960415     C                   CLEAR                   DSLR33
112900960415     C                   MOVE      4             D33FSC
113000030730     C                   MOVE      v03ddc        Dtaeur
113100030730     C                   MOVE      dtaeur        Dtaiso
113200030730     C                   MOVE      dtaiso        D33DCH
113300960415     C*
113400990519     C                   MOVE      ARBAAS        WAA4              4 0
113500101222     C                   Z-ADD     135           §§§CAA
113600110107     C     Anm_Bolla     SETLL     FNANM01L                           34
113700930624     C     *IN34         DOWEQ     '0'
113800110107     C     Anm_Bolla     READE     FNANM01L                               34
113900960415     C     *IN34         IFEQ      *OFF
114000960415     C                   Z-ADD     ANMNR1        D33NRR
114100960415     C                   CALL      'FNLR33R'
114200960415     C                   PARM                    DSLR33
114300960415     C                   MOVE      '1'           W33R              1
114400960415     C                   END
114500960415     C                   ENDDO
114600950918     C*
114700101222     C                   Z-ADD     150           §§§CAA
114800110107     C     Anm_Bolla     SETLL     FNANM01L                           34
114900950915     C     *IN34         DOWEQ     '0'
115000110107     C     Anm_Bolla     READE     FNANM01L                               34
115100960415     C     *IN34         IFEQ      *OFF
115200960415     C                   Z-ADD     ANMNR1        D33NRR
115300960415     C                   CALL      'FNLR33R'
115400960415     C                   PARM                    DSLR33
115500960415     C                   MOVE      '1'           W33R              1
115600960415     C                   END
115700960415     C                   ENDDO
115800930623     C*
115900930623     C                   ENDSR
116000100514     C*--------------------------------------------------------------*
116100101222     C*  sr7r  particolarità consegna
116200100514     C*--------------------------------------------------------------*
116300100514     C     sr7r          BEGSR
116400100514     c*
116500100514     C                   eval      tblcod = '7R'
116600100514     C                   eval      tblkey = arbgma
116700100514     c                   eval      tblkut = 1
116800100514     C     ktab          chain     Tabel00f
116900100514     c                   if        %found(tabel00f)
117000100514     C                   MOVEL     TBLUNI        DS7R
117100100514     c                   end
117200100514     C*
117300100514     C                   ENDSR
117400030826     C*-----------------------------------------------------*
117500101222     C*  toglie sull'orm x particolarità RC
117600030826     C*-----------------------------------------------------*
117700030826     C     srorm         BEGSR
117800101222      *
117900101222      * Tolgo anche il Ritiro Contestuale alla consegna
118000100514     c                   clear                   ds7r
118100100514     c                   if        arbgma <> ' '
118200100514     c                   exsr      sr7r
118300100514     c                   end
118400101222     c                   if        §7RRC ='S'
118500030826     c                   clear                   fnlr99ds
118600030826     c                   eval      i99tla = 'L'
118700030826     c                   eval      i99aas = arbaas
118800030826     c                   eval      i99lnp = arblnp
118900030826     c                   eval      i99nrs = arbnrs
119000030826     c                   eval      i99nsp = arbnsp
119100071001     c                   move      v03ifp        i99fgs
119200080930     c                   z-add     arbndc        i99ndc
119300030826     c                   eval      i99ddc = arbddc
119400071001     c                   eval      i99CMT = 'N'
119500071001     c                   eval      i99CoMiT = '1'
119600101222      *
119700101222     c                   if        modalita = Togliere
119800101222     c                   eval      i99fao = 390
119900101222     c                   elseif    modalita = Mettere
120000101222     c                   eval      i99fao = 400
120100101222     c                   eval      i99npg = 4
120200101222     c                   eval      i99nftl= 99999
120300101222     c                   end
120400101222      *
120500030826     c                   call      'FNLR99R'
120600030826     c                   parm                    kpjba
120700030826     c                   parm                    fnlr99ds
120800030826     c                   end
120900101222      *
121000030826     C                   ENDSR
121100080416     C*----------------------------------------------------*
121200080416     C* se pda e tolgo o metto spedizione e fase PDC richiamo pgm per
121300080416     c* aggiornare i dati al PDA
121400080416     C*----------------------------------------------------*
121500080416     C     srpda         BEGSR
121600080416     c                   eval      co42FGS = dstfgs
121700080416     C                   eval      co42NDC = dstnfv
121800080416     c                   eval      co42ddc = dstdfv
121900080801     c                   eval      co42aas =  arbaas
122000080801     c                   eval      co42lnp  = arbLNP
122100080801     c                   eval      co42nrs  = arbNRS
122200080801     c                   eval      co42nsp  = arbNSP
122300080416     c                   movel(p)  fidg42ds      kpjbu
122400080416     c                   call      'FIDG42R'
122500080416     c                   parm                    kpjba
122600110104     c                   eval      Kpjbu = kpjbus
122700080416     c* ritorno errore ???
122800101213     C*
122900080416     C                   ENDSR
123000070531     C*-----------------------------------------------------*
123100101217     C* toglie distinta sul FIARG oppure la mette
123200070531     C*-----------------------------------------------------*
123300070531     C     srfiarg       BEGSR
123400101213     C*
123500101217     c     ARG_di_nuovo  tag
123600101217     C*
123700110107     c     Bolla_ARB     chain(e)  fiarg01l
123800110104     C*  x errore
123900110104     c                   eval      ARG_Allocato = NO
124000101217     c                   if        %error
124100101217      *    Problemi durante l'aggiornamento --> eseguire ROLLBACK
124200101217     c                   clear                   trul82ds
124300101222     c                   eval      ul82§rrn = arg_nrr
124400101222     c                   eval      ul82§fil = 'FIARG01L'
124500101222     c                   eval      ul82§win = 'S'
124600101222     c                   eval      ul82§f7  = 'S'
124700101222     c                   eval      ul82§num = 2
124800101222     c                   eval      ul82§att = 2
124900101222     c                   eval      ul82§mss = 'Si sta bloccando la Distinta: '
125000110104     c                             + %editc(dstnfv:'Z') +
125100101217     c                             ' SI PREGA DI USCIRE dal lavoro|'
125200101222     c                   Eval      UL82§msw = 'La Distinta '
125300110103     c                             + %editc(I98NDC:'Z') +  ' '
125400101222     c                             + ' non è manutenzionabile.'
125500101217      *
125600101217      * chiamo il pgm che manda il messaggio info all'utente
125700101217     c                   call(e)   'TRUL82R'
125800101217     c                   parm                    trul82ds
125900101217     C*
126000101222      * se non più Allocata  riaggancia il record
126100101222     c                   if        ul82§sts <> 'A'
126200101217     c                   goto      ARG_di_nuovo
126300110104     c                   else
126400110104     c                   eval      ARG_Allocato = SI
126500110104      * Distinta non disponibile.....
126600110104     c                   eval      O98msgERR = ERR(12)
126700110104     c                   seton                                          90
126800110104     c                   leavesr
126900110104     c                   endif
127000101220     C*
127100101217     c                   end
127200101217     C*
127300101217     C*----------- Se rimane in errore invia segnalazioni (MSG di errore)
127400101221     c                   if        not %Found(fiarg01l) and Modalita = Togliere
127500101217     c                                or
127600110104     c                             %Found(fiarg01l) and Modalita = Mettere and
127700110104     c                             argNDC > 0
127800101222      * Distinta non più disponibile.....
127900110104     c                   eval      O98msgERR = ERR(20)
128000101217     c                   seton                                          90
128100101217     c                   leavesr
128200101217     c                   end
128300101217     C*
128400101220     C*----- Quindi prosegue:
128500101220     C*- in aggiornamento
128600101217     c                   if        %found(fiarg01l)
128700110104     C* se deve togliere
128800110104     c                   if         Modalita = Togliere
128900100202     c                   eval      dargflo = argflo
129000101222     c                   clear                   §argattesa
129100100202     c                   eval      argflo = dargflo
129200101217      *
129300070531     C                   clear                   argfgs
129400070531     C                   clear                   argNDC
129500070531     C                   clear                   argddc
129600070531     C                   clear                   argstp
129700070531     C                   clear                   argpdc
129800070531     c                   clear                   argnftl
129900070531     c                   clear                   ARGDTVDIS
130000070531     c                   clear                   ARGHVDIS
130100070531     c                   clear                   argslb
130200110104     c                   elseIf     Modalita = Mettere
130300110104     C* se deve Mettere
130400110104     c                   move      arbLNA        ARGLNA
130500110104     c                   move      I98fgs        ARGFGS
130600110104     c                   move      I98ndc        ARGNDC
130700110104     c                   eval      ARGPDC     =  DSTPDR
130800110104     c                   eval      ARGDDC     =  DSTDFV
130900110104     c                   eval      ARGSTP     =  0
131000110104     c                   eval      ARGNFTL    =  99999
131100110104     c                   eval      ARGSLB     =  0
131200110104     c                   eval      ARGDTVDIS  = Dateu
131300110104     c                   eval      ARGHVDIS   = HHMMss
131400110104     c                   end
131500101220     c                   update    fiarg000
131600110104     C* se deve Mettere
131700101217     c                   else
131800110104      * in inserimento scrive ARG poi aggiorna ARB con la routine di quadratura
131900110104     c                   move      arbAAS        ARGAAS
132000110104     c                   move      arbLNP        ARGLNP
132100110104     c                   move      arbNRS        ARGNRS
132200110104     c                   move      arbNSP        ARGNSP
132300110104     c                   move      arbLNA        ARGLNA
132400110104     c                   move      I98fgs        ARGFGS
132500110104     c                   move      I98ndc        ARGNDC
132600110104     c                   eval      ARGPDC     =  DSTPDR
132700110104     c                   eval      ARGDDC     =  DSTDFV
132800110104     c                   eval      ARGSTP     =  0
132900110104     c                   eval      ARGNFTL    =  99999
133000110104     c                   eval      ARGSLB     =  0
133100110104     c                   eval      ARGDTVDIS  = Dateu
133200110104     c                   eval      ARGHVDIS   = HHMMss
133300110104     c                   write     fiarg000
133400110105     C*
133500110105     c                   endif
133600101217     c*
133700110105     C* esegue aggiornamento di ARB
133800110105     c                   clear                   fidg23ds
133900110105     c                   eval      d23CMTI = 'N'
134000110105     c                   eval      d23FGSI = I98fgs
134100110105     c                   eval      d23NFTL = 99999
134200110105     c                   eval      d23DFVI = dstDFV
134300110105     c                   eval      d23NFDI = I98ndc
134400110105     c                   eval      d23NFAI = I98ndc
134500110105     c                   eval      d23FPPI = dstFPP
134600110105     c                   eval      kpjbus = kpjbu
134700110105     c                   eval      kpjbu = fidg23ds
134800110105     c                   call      'FIDG23C'
134900110105     c                   parm                    kpjba
135000110105     c                   eval      fidg23ds = kpjbu
135100110105     c                   eval      kpjbu = kpjbus
135200110105      * se c'è stato un errore
135300110105     c                   if        d23erro <> *blanks
135400110105      * ?  Problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
135500110105     c                   eval      O98msgERR = ERR(07)
135600110105     c                   seton                                          90
135700110105     c                   leavesr
135800110105     c                   end
135900101220      *
136000070531     c                   endsr
136100101220     c**********************************************************************
136200101220     C     srinz         BEGSR
136300101220     c**********************************************************************
136400101220     C* Pulisco i campi che devo vidualizzare nel fm2
136500101220     C*
136600101220     C                   clear                   V03pis
136700101220     C                   MOVE      *year         V03ANS
136800101220     C                   Z-ADD     *ZEROS        V03LNP
136900101220     C                   Z-ADD     *ZEROS        V03NRS
137000101220     C                   Z-ADD     *ZEROS        V03NSP
137100101220     C                   Z-ADD     *ZEROS        V03ifp
137200101220     C                   Z-ADD     *ZEROS        V03ndc
137300101220     C                   Z-ADD     *ZEROS        V03ddc
137400101220     C                   Z-ADD     *ZEROS        V03pdr
137500101220     C                   MOVEL     *blanks       V03rsc
137600101220     C                   MOVEL     *blanks       V03fpp
137700101220      *
137800101220     c* richiamo il pgm per dati a PDA x chiudere in lr
137900101222     c                   if        §OGPDAcon <> ' ' and
138000101220     c                             (dstpda =  'C' or dstpda = 'E' or
138100101222     c                             §DSTTSTPDA= 'C' or
138200101222     c                             §DSTTSTPDA= 'E')
138300101220     c                   clear                   fidg42ds
138400101220     c                   eval      co42tla = 'C'
138500101220     c                   exsr      srpda
138600101220     c                   end
138700101220      *
138800101220     c                   endsr
138900101221     C*---------------------------------------------------------------*
139000101213**  err
139100101214Distinta non accessibile. Chiusa o Annullata                       01
139200101214Distinta non accessibile. Non usa il PDA.                          02
139300101214Data dist.anteriore o super.alla data odierna dei gg.ammessi       03
139400101221Linea arrivo spedizione non in gestione.                           04
139500101221Spedizione Inesistente. Non trovata in Arrivo.                     05
139600101213Distinta non accessibile. Allocata da altro lavoro.                06
139700101217Aggiornamento Distinta NON eseguito correttamente.                 07
139800101222Sped. impegnata da altro lavoro non è possibile Proseguire.        08
139900101214Parametri passati non in formato numerico o mancanti               09
140000101214Data distinta superiore alla data odierna dei gg.ammessi           10
140100101222La Distinta non è la stessa presente sulla Bolla                   11
140200101215Aggiornamento fallito. Bolla utilizzata da un altro lavoro.        12
140300101222La Bolla è già assegnata ad una altra Distinta                     13
140400101221Parametro Filiale non in formato numerico o mancante               14
140500101221Parametro Nr.Distinta non in formato numerico o mancante           15
140600101221Parametro Anno Spediz.non in formato numerico o mancante           16
140700101221Parametro LNP  Spediz.non in formato numerico o mancante           17
140800101221Parametro Num. Spediz.non in formato numerico o mancante           18
140900101222La Spedizione non è in distinta. Non è possibile toglierla.        19
141000110104Aggiornamento fallito. Bolla su altra distinta.                    20
