000100020116     h decedit('0,') datedit(*dmy.)  option(*nodebugio)
000200020115
000300020115      * TRMK70R *-----------------------------------------------------*
000400020115      *            CAMBIO PUNTO OPERATIVO POTENZIALI     - BATCH -    *
000500020115      *---------------------------------------------------------------*
000600020115
000700020116     ftncpo08l  uf   e           k disk
000800100922     fticps11l  if   e           k disk
000900090129     fazcpr00f  if   e             disk
001000020116     fqsysprt   o    f  132        printer oflind(*inof)
001100020116
001200020116     d keycpo          s                   like(cpocmm) inz
001300090129     d $aut            s               n
001400020116
001500020116     d data_eur        s               d   datfmt(*eur)
001600020116     d data_iso        s               d   datfmt(*iso)
001700090129     d dataoggi        s              8  0
001800020116
001900020116     d kpjba         e ds
002000020116     d fnlv13ds      e ds
002100020116     d tisi95ds      e ds
002200021104
002300021104      * Ds di riferimento al file esterno AzUte00F
002400021104     d AzuteDs       e ds                  ExtName(Azute00f)
002500021104      * Ds per dati organigramma
002600021104     d Ddatiute      e ds
002700021104      * Parametri per richiamo al pgm di controllo profilo utenti
002800021104     d Tibs34Ds      e ds                  Inz
002900020116
003000090129      // ----------------------------------------------------------------------
003100090129
003200090129      * se lancio da menù elaboro
003300090129     c                   if        $aut
003400090129     c                   exsr      elabora
003500090129     c                   else
003600090129
003700090129      * se automatico con la data corrente
003800090129      * verifico nel file versioni cappario BART
003900090129      * se c'è una versione con data decorrenza = data corrente
004000090129      * => se esiste elaboro
004100090129     c     *start        setll     azcpr00f
004200090129     c                   do        *hival
004300090129     c                   read      azcpr00f
004400090129     c                   if        %eof(azcpr00f)
004500090129     c                   leave
004600090129     c                   endif
004700090129     c                   if        cpratb <> *blanks
004800090129     c                   iter
004900090129     c                   endif
005000090129     c                   if        cprdde = dataoggi
005100090129     c                   exsr      elabora
005200090129     c                   leave
005300090129     c                   endif
005400090129     c                   enddo
005500090129
005600090129     c                   endif
005700090129
005800090129     c                   eval      *inlr = *on
005900090129
006000090129      // ----------------------------------------------------------------------
006100090129      // Elaboro
006200090129      // ----------------------------------------------------------------------
006300090129     c     elabora       begsr
006400090129
006500020116      * Leggo solo i potenziali con codice commerciale a 0
006600020116     c     keycpo        setll     tncpo08l
006700020116     c                   do        *hival
006800020116     c     keycpo        reade     tncpo08l
006900020116
007000020116      * fine file esco dal ciclo
007100020116     c                   if        %eof(tncpo08l)
007200020116     c                   leave
007300020116     c                   endif
007400020116
007500020117      * potenziali annullati - esclusi
007600020116     c                   if        cpoatb = 'A'
007700020116     c                   iter
007800020116     c                   endif
007900020116
008000020117      * potenziali con azioni - esclusi
008100110131     c**!!!              if        cpofls = *blanks and cpoclt = *blanks
008200100922     c     cpocpo        setll     ticps11l
008300100922     c                   if        %equal (ticps11l)
008400020116     c                   iter
008500020116     c                   endif
008600110131     c**!!!              endif
008700020116
008800020116      * cerco il nuovo p.o. di appartenenza in base al cappario
008900020116     c                   clear                   fnlv13ds
009000020116     c                   clear                   tisi95ds
009100020116     c                   movel     '7'           i95tcn
009200020116     c                   movel     cpocap        i95cap
009300020116     c                   movel     cpocit        i95loc
009400020116     c                   movel     cpoprv        i95prv
009500020116     c     *iso          movel     data_iso      i95dat
009600020116     c                   movel     'S'           i13af0
009700020116     c                   movel     'S'           i13cng
009800020116     c                   call      'FNLV13R'
009900020116     c                   parm                    kpjba
010000020116     c                   parm                    fnlv13ds
010100020116     c                   parm                    tisi95ds
010200020116
010300020116      * Se errore non aggiorno e stampo
010400020116      * testo anche o13ron .... xchè richiamando con tcn='7' potrei avere un falso
010500020116      * errore di 'dati modificati'
010600020116     c                   if        o13err <> *blanks and o13ron = *blanks and
010700020116     c                             o13roc =  *blanks and o13rop = *blanks and
010800020116     c                             o13rol =  *blanks
010900020116     c                   if        not *in99
011000020116     c                   except    testa
011100020116     c                   eval      *in99 = *on
011200020116     c                   endif
011300020116     c                   if        *inof and *in99
011400020116     c                   except    testa
011500020116     c                   endif
011600020116     c                   except    dett
011700020116     c
011800020116     c                   else
011900020116
012000020116      * Aggiorno solo se il p.o. di appartenenza è diverso dalla linea di arrivo
012100020116      * che ho trovato
012200020116     c                   if        cpoflt <> o95lna
012300020116     c                   eval      cpoflt = o95lna
012400040806     c     *iso          movel     data_iso      cpodtr
012500020116     c                   update    tncpo000
012600020116     c                   endif
012700020116
012800020116     c                   endif
012900020116
013000020116     c                   enddo
013100090129
013200090129     c                   endsr
013300020115
013400020115      *---------------------------------------------------------------*
013500020116      *              ROUTINE INIZIALE
013600020115      *---------------------------------------------------------------*
013700020115
013800020115     c     *inzsr        begsr
013900020115
014000020115     c     *entry        plist
014100020115     c                   parm                    kpjba
014200090129
014300090129      * se primo byte della kpjbu è impostato, vuol dire che il lancio è stato
014400090129      * fatto da menù quindi non devo controllare la versione del cappario
014500090129      * ma aggiornare come ha sempre fatto
014600090129     c                   eval      $aut = (%subst(kpjbu:1:1) = 'M')
014700021104
014800021104      * Reperisco i dati utente
014900021104     c     *Dtaara       Define    §azute        AzuteDs
015000021104     c     *Dtaara       Define    §datiute      Ddatiute
015100021104
015200021104     c                   In(e)     *Dtaara
015300021104     c                   If        %Error or Rsut = *Blanks
015400021104     c                   Call      'TIBS34R'
015500021104     c                   Parm                    Tibs34Ds
015600021104     c                   In        *Dtaara
015700021104     c                   EndIf
015800020116
015900020116      * Data del giorno
016000020116     c     *eur          move      *date         data_eur
016100020116     c                   move      data_eur      data_iso
016200090129
016300090129     c     *iso          movel     data_iso      dataoggi
016400020116
016500020116     c                   endsr
016600020116
016700020116     oqsysprt   e            testa            02
016800020116     o                                         +  5 '***  C.A.P. Inesistenti'
016900020116     o                                         +  1 'trovati nel file dei'
017000020116     o                                         +  1 'Potenziali - TNCPO00F -'
017100020116     o                                         +  1 '  ***'
017200020116     o                                         + 10 'TRMK70R'
017300020116     o                       udate         y   + 15
017400020116     o                                          128 'Pag.'
017500020116     o                       page          z    132
017600020116     o          e            testa            04
017700020116     o                                           10 'Potenziale'
017800020116     o                                           29 'Ragione Sociale'
017900020116     o                                           74 'Località'
018000020116     o                                          104 'C.A.P.'
018100020116     o                                          110 'Prov'
018200020116     o          e            dett        1
018300020116     o                       cpocpo              11
018400020116     o                       cporag              62
018500020116     o                       cpocit              95
018600020116     o                       cpocap             103
018700020116     o                       cpoprv             109
