000100070719     h DECEDIT('0,') DATEDIT(*DMY.)
000200070719      * FIOR62R *----------------------------------------------------*
000300070719      *         Lancio assegnazione automatica distinte ORM          *
000400070719      *--------------------------------------------------------------*
000500070719     fFior62D   cf   e             workstn
000600070719     fAzorg01L  if   e           k disk
000700070719     fFidst08l  if   e           k disk
000800070719      *--------------------------------------------------------------*
000900070719      *  Schiera errori
001000070719      *--------------------------------------------------------------*
001100070926     d err             s             70    dim(06) ctdata perrcd(1)
001200070719      *--------------------------------------------------------------*
001300070719      *  DS
001400070719      *--------------------------------------------------------------*
001500070719     d Kpjba         e ds
001600070719
001700070719      * Parametri per controllo data e trasform in giorni - XSRDA8 -
001800070719
001900070719     d Wlbdat          ds                  inz
002000070719     d  g02dat                 1      8  0
002100070719     d  g02inv                 9     16  0
002200070719     d  g02err                17     17
002300070719     d  g02tgi                18     22  0
002400070719
002500070719      * Ds per Fnlv50  - Controllo filiale gestione
002600070719
002700070719     d Fnlv50ds      e ds
002800070719
002900070719     d Og147         e ds                  INZ
003000070719     d Og150         e ds                  INZ
003001161108     d ddft          E DS
003002161108     d Tibs02ds      E DS
003100071119     d fior20ds      e ds
003200070719     d Fior62ds      e ds
003300070719     d Ddatiute      e ds
003400070719     d Azuteds       e ds                  extname(azute00f)
003500070719     d Tibs34ds      e ds                  inz
003600071109     d fnorpds       e ds                  extname(fnorp00f)
003700070719
003800070719      *--------------------------------------------------------------*
003900070719      *  RIEPILOGO INDICATORI
004000070719      *--------------------------------------------------------------*
004100070719      *  01 - abilito f18
004200070719      *  02 - tolgo protezione al campo P.O. gestione
004300070719      *  03 - tolgo protezione al campo Data Distinta
004400070719      *  10 - filiale abilitata al picking messaggerie
004500071109      *  15 - proposte da analizzare
004600070719      *  28 - errore generico
004700070719      *  29 - generico
004800070719      *  30 - generico operazioni i/o
004900071009      *  40 - errore a video
005000071009      *  43 - errore a video
005100071009      *  44 - errore a video
005200070719      *  50 - sproteggo filiale in gestione
005300070719      *  51 - posizione cursore Data distinta
005400070719      *  88 - errore a vido
005500070719
005600070719      *--------------------------------------------------------------*
005700070719      *  VARIABILI
005800070719      *--------------------------------------------------------------*
005900070719     d knpg            s                   like(dstnpg)
006000070719     d kdfv            s                   like(dstdfv)
006100070719     d kfgs            s                   like(dstfgs)
006200070719     d knfv            s                   like(dstnfv)
006300070719
006400070719     d savddt          s                   like(wddt)
006500070719     d savfil          s                   like(v1cfil)
006600070719     d savtpm          s                   like(v1ctpm)
006700070926     d savfpp          s                   like(v1cfpp)
006800070719     d Wsvfil          s                   like(v1cfil)
006900070719     d Wddt            s                   like(dstdfv)
007000070719
007100070719     d Wfine           s              1
007200070719     d Whhdat          s             14  0
007300070719     d Woggi           s              8  0
007400070719
007500070719      *--------------------------------------------------------------*
007600070719      *  Ciclo principale
007700070719      *--------------------------------------------------------------*
007800070719      * Inizializzazione prima videata
007900070719     c                   Exsr      Inzd01
008000070719      * Loop di gestione videata
008100070720     c                   Dow       Wfine <> 'S'
008200071109      * controllo se per la filiale gestione ci sono delle proposte variazioni
008300071109      * ancora da analizzare
008400071109     c                   exsr      sr_controrp
008500070719     c                   exsr      gesd01
008600070719     c                   enddo
008700070719      * Fine Pgm.
008800070719     c                   seton                                        LR
008900070719      *--------------------------------------------------------------*
009000070719      *  Inizializzazione parametri prima videata
009100070719      *--------------------------------------------------------------*
009200070720     c     Inzd01        Begsr
009300070719      *
009400161111     c                   setoff                                       0302
009500070926     C                   clear                   V1Cfpp
009600070719     c                   eval      V1cclear  = 'N'
009700070719     c                   eval      V1cclrman = 'N'
009800070719     c                   z-add     Whhdat        V1cddc
009900070719      * verifico se P.o. già partito con la procedura distinta automatica
010000070719     c                   exsr      srorg
010100070719     c                   move      V1cfil        Wsvfil
010200070719      *
010300070719     c                   Endsr
010400070719      *--------------------------------------------------------------*
010500070719      *  Verifico se P.O. partito conn procedura automatica ORM
010600070719      *--------------------------------------------------------------*
010700070719     c     srorg         Begsr
010800070719
010900070719     c                   clear                   og147
011000070719     c                   clear                   og150
011100070719      * leggo organigramma
011200070719     c     v1cfil        chain     azorg01l
011300070719     c                   if        %found(azorg01l)
011400070719     c                   movel     orgde7        og147
011500070913     c                   movel     orgdf0        og150
011600070719     c                   endif
011700070719      * se p.o. non partito esco
011800070719     c                   if        §ogddao = *blanks
011900070313     c                   seton                                        lr
012000070313     c                   return
012100070313     c                   end
012200070719      * verifico se P.O. abilitato al picking messaggerie
012300070719     c                   eval      *in10 = (§ogpkm ='S')
012400070719
012500070926      * verifico se P.O. abilitato alla richiesta del tipo uscita distinta
012600070926     c                   eval      *in11 = (§ogfrtuo ='S')
012700070926
012800070719     c                   Endsr
012900071109
013000071109      *------------------------------------------------------------------------*
013100071109      *   Controllo se ci sono proposte da analizzare              ?
013200071109      *------------------------------------------------------------------------*
013300071109     c     sr_controrp   begsr
013400071109
013500071109     c                   eval      *in15 = *off
013600071109
013700071109     C/EXEC SQL
013800071109     C+ DECLARE A1 CURSOR FOR SELECT fnorp00f.*
013900071109     C+ FROM fnorp00f
014000071109     C+ WHERE orppor=:v1cfil and orpfev = ' '
014100071109     C/END-EXEC
014200071109
014300071109     C/EXEC SQL
014400071109     C+ OPEN A1
014500071109     C/END-EXEC
014600071109
014700071109     c                   do        *hival
014800071109
014900071109     C/EXEC SQL
015000071109     C+ FETCH NEXT FROM A1 INTO :fnorpds
015100071109     C/END-EXEC
015200071109
015300071109     c                   if        sqlcod = 100 or sqlcod < 0
015400071109     c                   leave
015500071109     c                   endif
015600071109
015700071109     c                   eval      *in15 = *on
015800071109     c                   leave
015900071109     c                   enddo
016000071109
016100071109     C/EXEC SQL
016200071109     C+ CLOSE A1
016300071109     C/END-EXEC
016400071109
016500071109     c                   endsr
016600071109
016700070719      *--------------------------------------------------------------*
016800070719      *  Gestione prima videata
016900070719      *--------------------------------------------------------------*
017000070719     c     Gesd01        Begsr
017100070719
017200070719     c                   do
017300070719
017400070719     c                   Exfmt     or62d01
017500070926     c                   setoff                                       285147
017600070719     c                   setoff                                       404344
017700070719      *  Fine Pgm.
017800070719     c                   If        *inkc
017900070719     c                   eval      wfine = 'S'
018000070719     c                   leave
018100070719     c                   endif
018200070719
018300070719      *  Sblocco data
018400070719     c                   If        *inka
018500070719     c                   seton                                        5103
018600070719     c                   setoff                                       29
018700070719     c                   iter
018800070719     c                   endif
018900071119      *  Gestione Proposte
019000071119     c                   if        *inkg
019100071119     c                   exsr      f07_proposte
019200071119     c                   iter
019300071119     c                   endif
019400070719      *  Cambio filiale
019500070719     c                   If        *inks
019600070719     c                   seton                                        5002
019700070719     c                   setoff                                       29
019800070719     c                   iter
019900070719     c                   endif
020000070719      *  Controlli
020100070719     c                   exsr      ctr01
020200070719      *  Conferma
020300070719     c                   If        not *in28 and *inkf
020400070719     c                   exsr      sbmp60
020500070719     c                   if        not *in28
020600070719     c                   movel     'S'           Wfine
020700070719     c                   endif
020800070719     c                   leave
020900070719     c                   endif
021000070719
021100070719     c                   enddo
021200070719      *
021300070719     c                   endsr
021400070719      *--------------------------------------------------------------*
021500070719      *  Controlli prima videata
021600070719      *--------------------------------------------------------------*
021700070719     c     Ctr01         BEGSR
021800070719
021900070719do  1c                   Do
022000070719      *  Controllo filiale
022100070720if  2c                   If        *in50 or wsvfil <> v1cfil
022200070719      * controllo la filiale gestione
022300070719     c                   clear                   Fnlv50ds
022400070719     c                   eval      d50pru = knmus
022500070720     c                   eval      d50fgs = v1cfil
022600070719     c                   call      'FNLV50R'
022700070719     c                   parm                    fnlv50ds
022800080215     c                   setoff                                       50
022900070719     c                   eval      wsvfil = v1cfil
023000070719
023100070719if  3c                   If        d50err <> ' '
023200070719     c                   seton                                        2840
023300070719     c                   movel     d50msg        $msg
023400070719     c                   leave
023500070719e   3c                   Endif
023501161108      * recupero i dati standard della nuova filiale
023502161108     c                   exsr      Rec_dft
023600070719      * verifico se filiale abilitata a distinta automatica e messaggerie
023700070719     c                   exsr      srorg
023800070719     c                   leave
023900070719e   2c                   endif
024000070719      *  Data distinta
024100070719if  2c                   If        V1cddc  = 0
024200070719     c                   seton                                        4328
024300070719     c                   movel     err(3)        $msg
024400070719     c                   leave
024500070719x   2c                   else
024600070719     c                   move      v1cddc        g02dat
024700070719     c                   clear                   g02err
024800070719     c                   call      'XSRDA8'
024900070719     c                   parm                    wlbdat
025000070719if  3c                   If        g02err = '1'
025100070719     c                   seton                                        2843
025200070719     c                   movel     err(2)        $msg
025300070719     c                   leave
025400070719e   3c                   endif
025500070719e   2c                   endif
025600070719
025700070719     c                   z-add     g02inv        Wddt
025800070719     c                   z-add     g02dat        V1cddc
025900070719      * la data distinta deve essere >= oggi
026000070802if  2c                   if        wddt < woggi and *in88 = *off                no edp
026100070719     c                   seton                                        2843
026200070719     c                   movel     err(2)        $msg
026300070719     c                   leave
026400070719e   2c                   endif
026500070719      *
026600070719     c                   z-add     4             Knpg
026700070719     c                   z-add     v1cfil        Kfgs
026800070719     c                   z-add     wddt          Kdfv
026900070719     c     Kdst2         chain     Fidst08l
027000070719if  2c                   If        not %found(Fidst08l)
027100070719     c                   seton                                        4328
027200070719     c                   movel     err(1)        $msg
027300070719     c                   leave
027400070719e   2c                   Endif
027500070926     c* se abilitato in organigramma matt/pom. obbligo l'inserimento
027600070926     c* del valore
027700070926     c                   if        §OGfrtuo = 'S' and  v1cfpp = ' '
027800070926     c                   seton                                        4728
027900070926     c                   movel     ERR(06)       $MSG
028000070926     c                   leave
028100070926     c                   end
028200070719      * se richiesta pulizia precedente assegnazione segnalo se una o +
028300070719      * distinte sono state stampate
028400070719if  2c                   if        V1CCLEAR = 'S' and not *in28
028500070719if  3c                   if        v1ctpm <> savtpm or v1cfil <> savfil
028600070926     c                             or wddt <> savddt or v1cfpp <> savfpp
028700070719     c                   setoff                                       29
028800070926     c                   eval      savfpp = v1cfpp
028900070719     c                   eval      savtpm = v1ctpm
029000070719     c                   eval      savfil = v1cfil
029100070719     c                   eval      savddt = wddt
029200070719e   3c                   end
029300070719if  3c                   if        not *in29
029400070719     c     kdst2         setll     fidst08l
029500070719d   4c                   do        *hival
029600070719     c     kdst2         reade     Fidst08l
029700070719if  5c                   if        %eof(fidst08l)
029800070719     c                   leave
029900070719e   5c                   end
030000070720if  5c                   if        dstrfasd = 'STP'
030100070719
030200070720if  6c                   if        (v1ctpm = 'P' and dsttpm = ' ') or
030300070719     c                             (v1ctpm = 'M' and dsttpm ='M') or
030400070719     c                             v1ctpm = ' '
030500070719     c                   seton                                        2928
030600070719     c                   movel     err(05)       $msg
030700070719     c                   leave
030800070720e   6c                   end
030900070719
031000070926if  6c                   if        (v1cfpp = 'P' and dstfpp = 'P') or
031100070926     c                             (v1cfpp = 'M' and dstfpp ='M') or
031200070926     c                              v1cfpp= ' '
031300070719     c                   seton                                        2928
031400070719     c                   movel     err(05)       $msg
031500070719     c                   leave
031600070720e   6c                   end
031700070720e   5c                   end
031800070719e   4C                   enddo
031900070719    3c                   end
032000070719    2c                   end
032100070719
032200070719    1C                   enddo
032300070719
032400070719     c                   Endsr
032500071119
032600071119      *------------------------------------------------------------------------*
032700071119      *   Gestione F07
032800071119      *------------------------------------------------------------------------*
032900071119     c     f07_proposte  begsr
033000071119
033100071119     c                   clear                   fior20ds
033200071119     c                   eval      i20fgs = v1cfil
033300071119     c                   eval      i20data = v1cddc
033400071119     c                   call      'FIOR20R'
033500071119     c                   parm                    kpjba
033600071119     c                   parm                    fior20ds
033700071119
033800071119     c                   endsr
033900071119
034000070719      *--------------------------------------------------------------*
034100070719      *  Sottometto preparazione distinta di consegna batch
034200070719      *--------------------------------------------------------------*
034300070719     c     Sbmp60        Begsr
034400070719
034500070719     c                   clear                   fior62ds
034600070719     c                   eval      or62cmti = 'S'
034700070719     c                   eval      or62FGSi = v1cfil
034800070719     c                   z-add     Wddt          or62Dfvi
034900070719     c                   eval      or62cleari= v1cclear
035000070719     c                   eval      or62clrman= v1cclrman
035100070926     c                   movel     v1cfpp        or62fppi
035200070719     c                   movel     v1ctpm        or62tpmi
035300071009     c                   movel     v1coca        or62allega
035301161108     c                   movel     v1co5k        or62finoa
035400070719     c                   movel(p)  Fior62DS      Kpjbu
035500070719     c                   call      'FIOR63C'
035600070719     c                   parm                    Kpjba
035700070719     c                   eval      Fior62ds =   Kpjbu
035800070719
035900070719     c                   if        or62erro<> ' '
036000070719     c                   seton                                        28
036100070719     c                   movel     err(4)        $msg
036200070719     c                   end
036300070719
036400070719     c                   Endsr
036401161108      *--------------------------------------------------------------*
036402161108      *  Recupero dati dalla tabella DFT (peso limite)
036403161108      *--------------------------------------------------------------*
036404161108     c     Rec_dft       Begsr
036405161108
036406161108      * Recupero i dati di default prima con il mio p.o.
036408161108     C                   clear                   ddft
036409161108     C                   clear                   TIBS02DS
036410161108     C                   movel     'C'           T02mod
036411161108     C                   movel     knsif         t02sif
036412161108     C                   movel     'DFT'         t02cod
036413161108     C                   movel(p)  v1cfil        T02ke1
036414161108     C                   call      'TIBS02R'
036415161108     C                   parm                    KPJBA
036416161108     C                   parm                    TIBS02DS
036417161108     C                   if        t02err =  *blanks
036418161108     C                   movel     t02uni        ddft
036419161108     C                   else
036420161108      * poi con 999 generico se non trovo con il mio p.o.
036421161108     C                   clear                   TIBS02DS
036422161108     C                   movel     'C'           T02mod
036423161108     C                   movel     knsif         t02sif
036424161108     C                   movel     'DFT'         t02cod
036425161108     C                   movel(p)  '999'         T02ke1
036426161108     C                   call      'TIBS02R'
036427161108     C                   parm                    KPJBA
036428161108     C                   parm                    TIBS02DS
036429161108     C                   if        t02err =  *blanks
036430161108     C                   movel     t02uni        ddft
036431161108     C                   endif
036432161108     C                   endif
036433161117      * valorizzo il campo kg limite del video
036434161117     c                   z-add     D§DFTvidpk    Vkglim
036441161108
036442161108     c                   Endsr
036500070719      *--------------------------------------------------------------*
036600070719      *  Operazioni iniziali
036700070719      *--------------------------------------------------------------*
036800070719     c     *Inzsr        Begsr
036900070719
037000070719     c     *Entry        Plist
037100070719     c                   Parm                    Kpjba
037200070719      * per EDP abilito pulizia orm  manuali
037300070719     c                   if        %subst(knmus: 1: 3) = 'EDP'
037400070719     c                   seton                                        88
037500070719     c                   else
037600070719     c                   setoff                                       88
037700070719     c                   end
037800070719      *------------------------------------------------------*
037900070719     c     *dtaara       define    §azute        azuteds
038000070719     c     *dtaara       define    §datiute      ddatiute
038100070719     c                   in(E)     *dtaara
038200070719     c                   If        %Error  or  RSUT = *blanks
038300070719     c                   call      'TIBS34R'
038400070719     c                   parm                    Tibs34Ds
038500070719     c                   in        *dtaara
038600070719     c                   endif
038700070719      *------------------------------------------------------*
038800070719      *  Definisco chiavi di accesso
038900070719     c     Kdst2         Klist
039000070719     c                   Kfld                    Kfgs
039100070719     c                   Kfld                    Knpg
039200070719     c                   Kfld                    Kdfv
039300070719      *------------------------------------------------------*
039400070719      *  Inizializzo campi
039500070719     c                   clear                   Fnlv50ds
039600070719     c                   clear                   Fior62ds
039700070719     c                   z-add     4             Knpg
039800070719     c                   movel     'N'           Wfine
039900070719
040000070719      * DATA DEL GIORNO
040100070719     c                   Time                    Whhdat           14 0
040200070719     c                   move      Whhdat        G02dat
040300070719     c                   movel     *blanks       G02err
040400070719     c                   call      'XSRDA8'
040500070719     c                   parm                    Wlbdat
040600070719     c                   move      G02inv        Woggi
040700070719
040800070719      * VEDO SE SONO SIMFEL O REMOTO
040900070719     c                   If        dutlpo = '2'
041000070719     c                   movel     dutpou        V1cfil
041100070719     c                   else
041200070719     c                   seton                                        01
041300070719     c                   movel     Simfel        V1cfil
041400070719     c                   endif
041500071010
041600071010      * valorizzo il campo degli ORM con allegati
041700071024     c                   eval      v1coca = ' '
041800070719
041801161108      * valorizzo il valore dei limiti kg per estrazioni ristrette
041802161108      * verifico se c'è in tabella DFT
041805161108     c                   exsr      Rec_dft
041831161108
041832161108     c                   eval      v1co5k = ' '
041833161108
041900070719     c                   Endsr
042000950524**
042100070719Non trovate distinte entro i limiti indicati                          01
042200070719Data errata                                                           02
042300070719Data distinta obbligatoria                                            03
042400070719L'elaborazione è terminata con errore ... controllare la stampa       04
042500070719Ci sono LDV già stampate. Le relative distinte non verranno pulite    05
042600070926Obbligatorio scegliere il tipo uscita                                 06
