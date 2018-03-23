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
003000020116
003100090129      // ----------------------------------------------------------------------
003200090129
003300090129      * se lancio da menù elaboro
003400090129     c                   if        $aut
003500090129     c                   exsr      elabora
003600090129     c                   else
003700090129
003800090129      * se automatico con la data corrente
003900090129      * verifico nel file versioni cappario BART
004000090129      * se c'è una versione con data decorrenza = data corrente
004100090129      * => se esiste elaboro
004200090129     c     *start        setll     azcpr00f
004300090129     c                   do        *hival
004400090129     c                   read      azcpr00f
004500090129     c                   if        %eof(azcpr00f)
004600090129     c                   leave
004700090129     c                   endif
004800090129     c                   if        cpratb <> *blanks
004900090129     c                   iter
005000090129     c                   endif
005100090129     c                   if        cprdde = dataoggi
005200090129     c                   exsr      elabora
005300090129     c                   leave
005400090129     c                   endif
005500090129     c                   enddo
005600090129
005700090129     c                   endif
005800090129
005900090129     c                   eval      *inlr = *on
006000090129
006100090129      // ----------------------------------------------------------------------
006200090129      // Elaboro
006300090129      // ----------------------------------------------------------------------
006400090129     c     elabora       begsr
006500090129
006600020116      * Leggo solo i potenziali con codice commerciale a 0
006700020116     c     keycpo        setll     tncpo08l
006800020116     c                   do        *hival
006900020116     c     keycpo        reade     tncpo08l
007000020116
007100020116      * fine file esco dal ciclo
007200020116     c                   if        %eof(tncpo08l)
007300020116     c                   leave
007400020116     c                   endif
007500020116
007600020117      * potenziali annullati - esclusi
007700020116     c                   if        cpoatb = 'A'
007800020116     c                   iter
007900020116     c                   endif
008000120919
008100120919      * potenziali già contattati - esclusi
008200120919     c                   eval      dcpo01 = CPOrst
008300120919     c                   IF        §CPOdtpcon > *zeros or §CPOdtpcon <> *blanks
008400120919     c                   iter
008500120919     c                   ENDIF
008600020116
008700020116      * cerco il nuovo p.o. di appartenenza in base al cappario
008800020116     c                   clear                   fnlv13ds
008900020116     c                   clear                   tisi95ds
009000020116     c                   movel     '7'           i95tcn
009100020116     c                   movel     cpocap        i95cap
009200020116     c                   movel     cpocit        i95loc
009300020116     c                   movel     cpoprv        i95prv
009400020116     c     *iso          movel     data_iso      i95dat
009500020116     c                   movel     'S'           i13af0
009600020116     c                   movel     'S'           i13cng
009700020116     c                   call      'FNLV13R'
009800020116     c                   parm                    kpjba
009900020116     c                   parm                    fnlv13ds
010000020116     c                   parm                    tisi95ds
010100020116
010200020116      * Se errore non aggiorno e stampo
010300020116      * testo anche o13ron .... xchè richiamando con tcn='7' potrei avere un falso
010400020116      * errore di 'dati modificati'
010500020116     c                   if        o13err <> *blanks and o13ron = *blanks and
010600020116     c                             o13roc =  *blanks and o13rop = *blanks and
010700020116     c                             o13rol =  *blanks
010800020116     c                   if        not *in99
010900020116     c                   except    testa
011000020116     c                   eval      *in99 = *on
011100020116     c                   endif
011200020116     c                   if        *inof and *in99
011300020116     c                   except    testa
011400020116     c                   endif
011500020116     c                   except    dett
011600020116     c
011700020116     c                   else
011800020116
011900020116      * Aggiorno solo se il p.o. di appartenenza è diverso dalla linea di arrivo
012000020116      * che ho trovato
012100020116     c                   if        cpoflt <> o95lna
012200020116     c                   eval      cpoflt = o95lna
012300040806     c     *iso          movel     data_iso      cpodtr
012400020116     c                   update    tncpo000
012500020116     c                   endif
012600020116
012700020116     c                   endif
012800020116
012900020116     c                   enddo
013000090129
013100090129     c                   endsr
013200020115
013300020115      *---------------------------------------------------------------*
013400020116      *              ROUTINE INIZIALE
013500020115      *---------------------------------------------------------------*
013600020115
013700020115     c     *inzsr        begsr
013800020115
013900020115     c     *entry        plist
014000020115     c                   parm                    kpjba
014100090129
014200090129      * se primo byte della kpjbu è impostato, vuol dire che il lancio è stato
014300090129      * fatto da menù quindi non devo controllare la versione del cappario
014400090129      * ma aggiornare come ha sempre fatto
014500090129     c                   eval      $aut = (%subst(kpjbu:1:1) = 'M')
014600021104
014700021104      * Reperisco i dati utente
014800021104     c     *Dtaara       Define    §azute        AzuteDs
014900021104     c     *Dtaara       Define    §datiute      Ddatiute
015000021104
015100021104     c                   In(e)     *Dtaara
015200021104     c                   If        %Error or Rsut = *Blanks
015300021104     c                   Call      'TIBS34R'
015400021104     c                   Parm                    Tibs34Ds
015500021104     c                   In        *Dtaara
015600021104     c                   EndIf
015700020116
015800020116      * Data del giorno
015900020116     c     *eur          move      *date         data_eur
016000020116     c                   move      data_eur      data_iso
016100090129
016200090129     c     *iso          movel     data_iso      dataoggi
016300020116
016400020116     c                   endsr
016500020116
016600020116     oqsysprt   e            testa            02
016700020116     o                                         +  5 '***  C.A.P. Inesistenti'
016800020116     o                                         +  1 'trovati nel file dei'
016900020116     o                                         +  1 'Potenziali - TNCPO00F -'
017000020116     o                                         +  1 '  ***'
017100020116     o                                         + 10 'TRMK70R'
017200020116     o                       udate         y   + 15
017300020116     o                                          128 'Pag.'
017400020116     o                       page          z    132
017500020116     o          e            testa            04
017600020116     o                                           10 'Potenziale'
017700020116     o                                           29 'Ragione Sociale'
017800020116     o                                           74 'Località'
017900020116     o                                          104 'C.A.P.'
018000020116     o                                          110 'Prov'
018100020116     o          e            dett        1
018200020116     o                       cpocpo              11
018300020116     o                       cporag              62
018400020116     o                       cpocit              95
018500020116     o                       cpocap             103
018600020116     o                       cpoprv             109
