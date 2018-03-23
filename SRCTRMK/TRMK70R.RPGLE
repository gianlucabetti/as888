000100020116     h decedit('0,') datedit(*dmy.)  option(*nodebugio)
000200020115
000300020115      * TRMK70R *-----------------------------------------------------*
000400020115      *            CAMBIO PUNTO OPERATIVO POTENZIALI     - BATCH -    *
000500020115      *---------------------------------------------------------------*
000600020115
000700020116     ftncpo08l  uf   e           k disk
000800090129     fazcpr00f  if   e             disk
000900020116     fqsysprt   o    f  132        printer oflind(*inof)
001000020116
001100020116     d keycpo          s                   like(cpocmm) inz
001200090129     d $aut            s               n
001300020116
001400020116     d data_eur        s               d   datfmt(*eur)
001500020116     d data_iso        s               d   datfmt(*iso)
001600090129     d dataoggi        s              8  0
001700020116
001800020116     d kpjba         e ds
001900020116     d fnlv13ds      e ds
002000020116     d tisi95ds      e ds
002100120919
002200120919     d DCPO01        e ds
002300021104
002400021104      * Ds di riferimento al file esterno AzUte00F
002500021104     d AzuteDs       e ds                  ExtName(Azute00f)
002600021104      * Ds per dati organigramma
002700021104     d Ddatiute      e ds
002800021104      * Parametri per richiamo al pgm di controllo profilo utenti
002900021104     d Tibs34Ds      e ds                  Inz
003000160804
003100160804       // -?Storicizzazione variazioni
003200160804     d TRMK30DS      e ds
003300160804     d TNCPO_30      e ds                  extname(TNCPO00F) inz
003400160804     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
003500160804     d TICPI_30      e ds                  extname(TICPI00F) inz
003600160804
003700160804      //---------------------------------------------------------------
003800160804      //?Definizione procedure esterne.
003900160804      //---------------------------------------------------------------
004000160804       // -?Storicizzazione Variazioni Potenziale
004100160804     d TRMK30R         pr                  extpgm('TRMK30R')
004200160804     d  trmk30ds                           likeds(trmk30ds)
004300160804     d  tncpo_30                           likeds(tncpo_30)
004400160804     d  tncpo1_30                          likeds(tncpo1_30)
004500160804     d  ticpi_30                           likeds(ticpi_30)
004600020116
004700090129      // ----------------------------------------------------------------------
004800090129
004900090129      * se lancio da menù elaboro
005000090129     c                   if        $aut
005100090129     c                   exsr      elabora
005200090129     c                   else
005300090129
005400090129      * se automatico con la data corrente
005500090129      * verifico nel file versioni cappario BART
005600090129      * se c'è una versione con data decorrenza = data corrente
005700090129      * => se esiste elaboro
005800090129     c     *start        setll     azcpr00f
005900090129     c                   do        *hival
006000090129     c                   read      azcpr00f
006100090129     c                   if        %eof(azcpr00f)
006200090129     c                   leave
006300090129     c                   endif
006400090129     c                   if        cpratb <> *blanks
006500090129     c                   iter
006600090129     c                   endif
006700090129     c                   if        cprdde = dataoggi
006800090129     c                   exsr      elabora
006900090129     c                   leave
007000090129     c                   endif
007100090129     c                   enddo
007200090129
007300090129     c                   endif
007400160804
007500160804      /free
007600160804       //?Chiudo file storicizzazione variazioni potenziale
007700160804         clear TRMK30DS;
007800160804         clear tncpo_30;
007900160804         clear tncpo1_30;
008000160804         clear ticpi_30;
008100160804         IMK30tla = 'C';
008200160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
008300160804      /end-free
008400090129
008500090129     c                   eval      *inlr = *on
008600090129
008700090129      // ----------------------------------------------------------------------
008800090129      // Elaboro
008900090129      // ----------------------------------------------------------------------
009000090129     c     elabora       begsr
009100090129
009200020116      * Leggo solo i potenziali con codice commerciale a 0
009300020116     c     keycpo        setll     tncpo08l
009400020116     c                   do        *hival
009500020116     c     keycpo        reade     tncpo08l
009600020116
009700020116      * fine file esco dal ciclo
009800020116     c                   if        %eof(tncpo08l)
009900020116     c                   leave
010000020116     c                   endif
010100020116
010200020117      * potenziali annullati - esclusi
010300020116     c                   if        cpoatb = 'A'
010400020116     c                   iter
010500020116     c                   endif
010600120919
010700120919      * potenziali già contattati - esclusi
010800120919     c                   eval      dcpo01 = CPOrst
010900120919     c                   IF        §CPOdtpcon > *zeros or §CPOdtpcon <> *blanks
011000120919     c                   iter
011100120919     c                   ENDIF
011200020116
011300020116      * cerco il nuovo p.o. di appartenenza in base al cappario
011400020116     c                   clear                   fnlv13ds
011500020116     c                   clear                   tisi95ds
011600020116     c                   movel     '7'           i95tcn
011700020116     c                   movel     cpocap        i95cap
011800020116     c                   movel     cpocit        i95loc
011900020116     c                   movel     cpoprv        i95prv
012000020116     c     *iso          movel     data_iso      i95dat
012100020116     c                   movel     'S'           i13af0
012200020116     c                   movel     'S'           i13cng
012300020116     c                   call      'FNLV13R'
012400020116     c                   parm                    kpjba
012500020116     c                   parm                    fnlv13ds
012600020116     c                   parm                    tisi95ds
012700020116
012800020116      * Se errore non aggiorno e stampo
012900020116      * testo anche o13ron .... xchè richiamando con tcn='7' potrei avere un falso
013000020116      * errore di 'dati modificati'
013100020116     c                   if        o13err <> *blanks and o13ron = *blanks and
013200020116     c                             o13roc =  *blanks and o13rop = *blanks and
013300020116     c                             o13rol =  *blanks
013400020116     c                   if        not *in99
013500020116     c                   except    testa
013600020116     c                   eval      *in99 = *on
013700020116     c                   endif
013800020116     c                   if        *inof and *in99
013900020116     c                   except    testa
014000020116     c                   endif
014100020116     c                   except    dett
014200020116     c
014300020116     c                   else
014400160804      /free
014500160804       //?Salvo l'immagine precedente del potenziale
014600160804         exsr ImmaginePrima;
014700160804      /end-free
014800020116
014900020116      * Aggiorno solo se il p.o. di appartenenza è diverso dalla linea di arrivo
015000020116      * che ho trovato
015100020116     c                   if        cpoflt <> o95lna
015200020116     c                   eval      cpoflt = o95lna
015300040806     c     *iso          movel     data_iso      cpodtr
015400020116     c                   update    tncpo000
015500020116     c                   endif
015600160804      /free
015700160804       //?Scrivo la variazione fatta sul potenziale
015800160804         exsr ScriviVariazione;
015900160804      /end-free
016000020116
016100020116     c                   endif
016200020116
016300020116     c                   enddo
016400090129
016500090129     c                   endsr
016600020115
016700020115      *---------------------------------------------------------------*
016800020116      *              ROUTINE INIZIALE
016900020115      *---------------------------------------------------------------*
017000020115
017100020115     c     *inzsr        begsr
017200020115
017300020115     c     *entry        plist
017400020115     c                   parm                    kpjba
017500090129
017600090129      * se primo byte della kpjbu è impostato, vuol dire che il lancio è stato
017700090129      * fatto da menù quindi non devo controllare la versione del cappario
017800090129      * ma aggiornare come ha sempre fatto
017900090129     c                   eval      $aut = (%subst(kpjbu:1:1) = 'M')
018000021104
018100021104      * Reperisco i dati utente
018200021104     c     *Dtaara       Define    §azute        AzuteDs
018300021104     c     *Dtaara       Define    §datiute      Ddatiute
018400021104
018500021104     c                   In(e)     *Dtaara
018600021104     c                   If        %Error or Rsut = *Blanks
018700021104     c                   Call      'TIBS34R'
018800021104     c                   Parm                    Tibs34Ds
018900021104     c                   In        *Dtaara
019000021104     c                   EndIf
019100020116
019200020116      * Data del giorno
019300020116     c     *eur          move      *date         data_eur
019400020116     c                   move      data_eur      data_iso
019500090129
019600090129     c     *iso          movel     data_iso      dataoggi
019700020116
019800020116     c                   endsr
019900160804      /free
020000160804       //--------------------------------------------------------------
020100160804       //?Scrivo l'immagine Precedente anagrafica Potenziale
020200160804       //--------------------------------------------------------------
020300160804       BEGSR ImmaginePrima;
020400160804
020500160804         clear TRMK30DS;
020600160804         IMK30immag = 'P';
020700160804         IMK30tvp = 'E';
020800160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
020900160804
021000160804       ENDSR;
021100160804
021200160804       //--------------------------------------------------------------
021300160804       //?Scrivo Storico Variazioni.
021400160804       //--------------------------------------------------------------
021500160804       BEGSR ScriviVariazione;
021600160804
021700160804         clear TRMK30DS;
021800160804         IMK30pru = knmus;
021900160804         IMK30noj = knmeb;
022000160804         IMK30pgm = 'TRMK70R';
022100160804         IMK30immag = 'D';
022200160804         IMK30cta = 'M';
022300160804         IMK30tvp = 'E';
022400160804
022500160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
022600160804
022700160804       ENDSR;
022800160804      /end-free
022900020116
023000020116     oqsysprt   e            testa            02
023100020116     o                                         +  5 '***  C.A.P. Inesistenti'
023200020116     o                                         +  1 'trovati nel file dei'
023300020116     o                                         +  1 'Potenziali - TNCPO00F -'
023400020116     o                                         +  1 '  ***'
023500020116     o                                         + 10 'TRMK70R'
023600020116     o                       udate         y   + 15
023700020116     o                                          128 'Pag.'
023800020116     o                       page          z    132
023900020116     o          e            testa            04
024000020116     o                                           10 'Potenziale'
024100020116     o                                           29 'Ragione Sociale'
024200020116     o                                           74 'Località'
024300020116     o                                          104 'C.A.P.'
024400020116     o                                          110 'Prov'
024500020116     o          e            dett        1
024600020116     o                       cpocpo              11
024700020116     o                       cporag              62
024800020116     o                       cpocit              95
024900020116     o                       cpocap             103
025000020116     o                       cpoprv             109
