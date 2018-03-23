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
003100070719     d Fior62ds      e ds
003200070719     d Ddatiute      e ds
003300070719     d Azuteds       e ds                  extname(azute00f)
003400070719     d Tibs34ds      e ds                  inz
003500070719
003600070719      *--------------------------------------------------------------*
003700070719      *  RIEPILOGO INDICATORI
003800070719      *--------------------------------------------------------------*
003900070719      *  01 - abilito f18
004000070719      *  02 - tolgo protezione al campo P.O. gestione
004100070719      *  03 - tolgo protezione al campo Data Distinta
004200070719      *  10 - filiale abilitata al picking messaggerie
004300070719      *  28 - errore generico
004400070719      *  29 - generico
004500070719      *  30 - generico operazioni i/o
004600071009      *  40 - errore a video
004700071009      *  43 - errore a video
004800071009      *  44 - errore a video
004900070719      *  50 - sproteggo filiale in gestione
005000070719      *  51 - posizione cursore Data distinta
005100070719      *  88 - errore a vido
005200070719
005300070719      *--------------------------------------------------------------*
005400070719      *  VARIABILI
005500070719      *--------------------------------------------------------------*
005600070719     d knpg            s                   like(dstnpg)
005700070719     d kdfv            s                   like(dstdfv)
005800070719     d kfgs            s                   like(dstfgs)
005900070719     d knfv            s                   like(dstnfv)
006000070719
006100070719     d savddt          s                   like(wddt)
006200070719     d savfil          s                   like(v1cfil)
006300070719     d savtpm          s                   like(v1ctpm)
006400070926     d savfpp          s                   like(v1cfpp)
006500070719     d Wsvfil          s                   like(v1cfil)
006600070719     d Wddt            s                   like(dstdfv)
006700070719
006800070719     d Wfine           s              1
006900070719     d Whhdat          s             14  0
007000070719     d Woggi           s              8  0
007100070719
007200070719      *--------------------------------------------------------------*
007300070719      *  Ciclo principale
007400070719      *--------------------------------------------------------------*
007500070719      * Inizializzazione prima videata
007600070719     c                   Exsr      Inzd01
007700070719      * Loop di gestione videata
007800070720     c                   Dow       Wfine <> 'S'
007900070719     c                   exsr      gesd01
008000070719     c                   enddo
008100070719      * Fine Pgm.
008200070719     c                   seton                                        LR
008300070719      *--------------------------------------------------------------*
008400070719      *  Inizializzazione parametri prima videata
008500070719      *--------------------------------------------------------------*
008600070720     c     Inzd01        Begsr
008700070719      *
008800070719     c                   setoff                                       0302
008900070926     C                   clear                   V1Cfpp
009000070719     c                   eval      V1cclear  = 'N'
009100070719     c                   eval      V1cclrman = 'N'
009200070719     c                   z-add     Whhdat        V1cddc
009300070719      * verifico se P.o. già partito con la procedura distinta automatica
009400070719     c                   exsr      srorg
009500070719     c                   move      V1cfil        Wsvfil
009600070719      *
009700070719     c                   Endsr
009800070719      *--------------------------------------------------------------*
009900070719      *  Verifico se P.O. partito conn procedura automatica ORM
010000070719      *--------------------------------------------------------------*
010100070719     c     srorg         Begsr
010200070719
010300070719     c                   clear                   og147
010400070719     c                   clear                   og150
010500070719      * leggo organigramma
010600070719     c     v1cfil        chain     azorg01l
010700070719     c                   if        %found(azorg01l)
010800070719     c                   movel     orgde7        og147
010900070913     c                   movel     orgdf0        og150
011000070719     c                   endif
011100070719      * se p.o. non partito esco
011200070719     c                   if        §ogddao = *blanks
011300070313     c                   seton                                        lr
011400070313     c                   return
011500070313     c                   end
011600070719      * verifico se P.O. abilitato al picking messaggerie
011700070719     c                   eval      *in10 = (§ogpkm ='S')
011800070719
011900070926      * verifico se P.O. abilitato alla richiesta del tipo uscita distinta
012000070926     c                   eval      *in11 = (§ogfrtuo ='S')
012100070926
012200070719     c                   Endsr
012300070719      *--------------------------------------------------------------*
012400070719      *  Gestione prima videata
012500070719      *--------------------------------------------------------------*
012600070719     c     Gesd01        Begsr
012700070719
012800070719     c                   do
012900070719
013000070719     c                   Exfmt     or62d01
013100070926     c                   setoff                                       285147
013200070719     c                   setoff                                       404344
013300070719      *  Fine Pgm.
013400070719     c                   If        *inkc
013500070719     c                   eval      wfine = 'S'
013600070719     c                   leave
013700070719     c                   endif
013800070719
013900070719      *  Sblocco data
014000070719     c                   If        *inka
014100070719     c                   seton                                        5103
014200070719     c                   setoff                                       29
014300070719     c                   iter
014400070719     c                   endif
014500070719      *  Cambio filiale
014600070719     c                   If        *inks
014700070719     c                   seton                                        5002
014800070719     c                   setoff                                       29
014900070719     c                   iter
015000070719     c                   endif
015100070719      *  Controlli
015200070719     c                   exsr      ctr01
015300070719      *  Conferma
015400070719     c                   If        not *in28 and *inkf
015500070719     c                   exsr      sbmp60
015600070719     c                   if        not *in28
015700070719     c                   movel     'S'           Wfine
015800070719     c                   endif
015900070719     c                   leave
016000070719     c                   endif
016100070719
016200070719     c                   enddo
016300070719      *
016400070719     c                   endsr
016500070719      *--------------------------------------------------------------*
016600070719      *  Controlli prima videata
016700070719      *--------------------------------------------------------------*
016800070719     c     Ctr01         BEGSR
016900070719
017000070719do  1c                   Do
017100070719      *  Controllo filiale
017200070720if  2c                   If        *in50 or wsvfil <> v1cfil
017300070719      * controllo la filiale gestione
017400070719     c                   clear                   Fnlv50ds
017500070719     c                   eval      d50pru = knmus
017600070720     c                   eval      d50fgs = v1cfil
017700070719     c                   call      'FNLV50R'
017800070719     c                   parm                    fnlv50ds
017900070719     c                   seton                                        50
018000070719     c                   eval      wsvfil = v1cfil
018100070719
018200070719if  3c                   If        d50err <> ' '
018300070719     c                   seton                                        2840
018400070719     c                   movel     d50msg        $msg
018500070719     c                   leave
018600070719e   3c                   Endif
018700070719      * verifico se filiale abilitata a distinta automatica e messaggerie
018800070719     c                   exsr      srorg
018900070719     c                   leave
019000070719e   2c                   endif
019100070719      *  Data distinta
019200070719if  2c                   If        V1cddc  = 0
019300070719     c                   seton                                        4328
019400070719     c                   movel     err(3)        $msg
019500070719     c                   leave
019600070719x   2c                   else
019700070719     c                   move      v1cddc        g02dat
019800070719     c                   clear                   g02err
019900070719     c                   call      'XSRDA8'
020000070719     c                   parm                    wlbdat
020100070719if  3c                   If        g02err = '1'
020200070719     c                   seton                                        2843
020300070719     c                   movel     err(2)        $msg
020400070719     c                   leave
020500070719e   3c                   endif
020600070719e   2c                   endif
020700070719
020800070719     c                   z-add     g02inv        Wddt
020900070719     c                   z-add     g02dat        V1cddc
021000070719      * la data distinta deve essere >= oggi
021100070802if  2c                   if        wddt < woggi and *in88 = *off                no edp
021200070719     c                   seton                                        2843
021300070719     c                   movel     err(2)        $msg
021400070719     c                   leave
021500070719e   2c                   endif
021600070719      *
021700070719     c                   z-add     4             Knpg
021800070719     c                   z-add     v1cfil        Kfgs
021900070719     c                   z-add     wddt          Kdfv
022000070719     c     Kdst2         chain     Fidst08l
022100070719if  2c                   If        not %found(Fidst08l)
022200070719     c                   seton                                        4328
022300070719     c                   movel     err(1)        $msg
022400070719     c                   leave
022500070719e   2c                   Endif
022600070926     c* se abilitato in organigramma matt/pom. obbligo l'inserimento
022700070926     c* del valore
022800070926     c                   if        §OGfrtuo = 'S' and  v1cfpp = ' '
022900070926     c                   seton                                        4728
023000070926     c                   movel     ERR(06)       $MSG
023100070926     c                   leave
023200070926     c                   end
023300070719      * se richiesta pulizia precedente assegnazione segnalo se una o +
023400070719      * distinte sono state stampate
023500070719if  2c                   if        V1CCLEAR = 'S' and not *in28
023600070719if  3c                   if        v1ctpm <> savtpm or v1cfil <> savfil
023700070926     c                             or wddt <> savddt or v1cfpp <> savfpp
023800070719     c                   setoff                                       29
023900070926     c                   eval      savfpp = v1cfpp
024000070719     c                   eval      savtpm = v1ctpm
024100070719     c                   eval      savfil = v1cfil
024200070719     c                   eval      savddt = wddt
024300070719e   3c                   end
024400070719if  3c                   if        not *in29
024500070719     c     kdst2         setll     fidst08l
024600070719d   4c                   do        *hival
024700070719     c     kdst2         reade     Fidst08l
024800070719if  5c                   if        %eof(fidst08l)
024900070719     c                   leave
025000070719e   5c                   end
025100070720if  5c                   if        dstrfasd = 'STP'
025200070719
025300070720if  6c                   if        (v1ctpm = 'P' and dsttpm = ' ') or
025400070719     c                             (v1ctpm = 'M' and dsttpm ='M') or
025500070719     c                             v1ctpm = ' '
025600070719     c                   seton                                        2928
025700070719     c                   movel     err(05)       $msg
025800070719     c                   leave
025900070720e   6c                   end
026000070719
026100070926if  6c                   if        (v1cfpp = 'P' and dstfpp = 'P') or
026200070926     c                             (v1cfpp = 'M' and dstfpp ='M') or
026300070926     c                              v1cfpp= ' '
026400070719     c                   seton                                        2928
026500070719     c                   movel     err(05)       $msg
026600070719     c                   leave
026700070720e   6c                   end
026800070720e   5c                   end
026900070719e   4C                   enddo
027000070719    3c                   end
027100070719    2c                   end
027200070719
027300070719    1C                   enddo
027400070719
027500070719     c                   Endsr
027600070719      *--------------------------------------------------------------*
027700070719      *  Sottometto preparazione distinta di consegna batch
027800070719      *--------------------------------------------------------------*
027900070719     c     Sbmp60        Begsr
028000070719
028100070719     c                   clear                   fior62ds
028200070719     c                   eval      or62cmti = 'S'
028300070719     c                   eval      or62FGSi = v1cfil
028400070719     c                   z-add     Wddt          or62Dfvi
028500070719     c                   eval      or62cleari= v1cclear
028600070719     c                   eval      or62clrman= v1cclrman
028700070926     c                   movel     v1cfpp        or62fppi
028800070719     c                   movel     v1ctpm        or62tpmi
028900071009     c                   movel     v1coca        or62allega
029000070719     c                   movel(p)  Fior62DS      Kpjbu
029100070719     c                   call      'FIOR63C'
029200070719     c                   parm                    Kpjba
029300070719     c                   eval      Fior62ds =   Kpjbu
029400070719
029500070719     c                   if        or62erro<> ' '
029600070719     c                   seton                                        28
029700070719     c                   movel     err(4)        $msg
029800070719     c                   end
029900070719
030000070719     c                   Endsr
030100070719      *--------------------------------------------------------------*
030200070719      *  Operazioni iniziali
030300070719      *--------------------------------------------------------------*
030400070719     c     *Inzsr        Begsr
030500070719
030600070719     c     *Entry        Plist
030700070719     c                   Parm                    Kpjba
030800070719      * per EDP abilito pulizia orm  manuali
030900070719     c                   if        %subst(knmus: 1: 3) = 'EDP'
031000070719     c                   seton                                        88
031100070719     c                   else
031200070719     c                   setoff                                       88
031300070719     c                   end
031400070719      *------------------------------------------------------*
031500070719     c     *dtaara       define    §azute        azuteds
031600070719     c     *dtaara       define    §datiute      ddatiute
031700070719     c                   in(E)     *dtaara
031800070719     c                   If        %Error  or  RSUT = *blanks
031900070719     c                   call      'TIBS34R'
032000070719     c                   parm                    Tibs34Ds
032100070719     c                   in        *dtaara
032200070719     c                   endif
032300070719      *------------------------------------------------------*
032400070719      *  Definisco chiavi di accesso
032500070719     c     Kdst2         Klist
032600070719     c                   Kfld                    Kfgs
032700070719     c                   Kfld                    Knpg
032800070719     c                   Kfld                    Kdfv
032900070719      *------------------------------------------------------*
033000070719      *  Inizializzo campi
033100070719     c                   clear                   Fnlv50ds
033200070719     c                   clear                   Fior62ds
033300070719     c                   z-add     4             Knpg
033400070719     c                   movel     'N'           Wfine
033500070719
033600070719      * DATA DEL GIORNO
033700070719     c                   Time                    Whhdat           14 0
033800070719     c                   move      Whhdat        G02dat
033900070719     c                   movel     *blanks       G02err
034000070719     c                   call      'XSRDA8'
034100070719     c                   parm                    Wlbdat
034200070719     c                   move      G02inv        Woggi
034300070719
034400070719      * VEDO SE SONO SIMFEL O REMOTO
034500070719     c                   If        dutlpo = '2'
034600070719     c                   movel     dutpou        V1cfil
034700070719     c                   else
034800070719     c                   seton                                        01
034900070719     c                   movel     Simfel        V1cfil
035000070719     c                   endif
035100071010
035200071010      * valorizzo il campo degli ORM con allegati
035300071010     c                   eval      v1coca = 'E'
035400070719
035500070719     c                   Endsr
035600950524**
035700070719Non trovate distinte entro i limiti indicati                          01
035800070719Data errata                                                           02
035900070719Data distinta obbligatoria                                            03
036000070719L'elaborazione è terminata con errore ... controllare la stampa       04
036100070719Ci sono LDV già stampate. Le relative distinte non verranno pulite    05
036200070926Obbligatorio scegliere il tipo uscita                                 06
