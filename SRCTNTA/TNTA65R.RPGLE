000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300050216      *        S T A M P A       L E T T E R A
000400060906      *        TESTO 38
000500020108      *------------------------------------------------------------------*
000600020107     FTABEL00F  IF   E           K DISK
000700070227     FTFACO00F  IF   E           K DISK    usropn
000800070227     FTFIND00F  IF   E           K DISK    usropn
000900091022     fTivis05l  if   e           k disk    usropn
001000060906     FTNTA65P   O    E             PRINTER oflind(*in80)
001100020108
001200020108      *------------------------------------------------------------------*
001300050216      *  D S   E S T E R N E
001400020108      *------------------------------------------------------------------*
001500050216     d TIBS69DS      e ds                  inz
001600050216     d ds_CNACO      e ds                  inz  extname(CNACO00F)
001700050216     d ds_CNIND      e ds                  inz  extname(CNIND00F)
001800050216     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
001900050216     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
002000020107      * DS DEL FILE TABEL00F
002100050216     D DS1H          E DS
002200020625      *--------
002300020625      * Ds di riferimento al file esterno AzUte00F
002400020625      *--------
002500020625     d AzuteDs       e ds                  ExtName(AzUte00F)
002600020625      *--------
002700020625      * Ds per dati organigramma
002800020625      *--------
002900020625     d DDatiUte      e ds
003000020625      *--------
003100020625      * Parametri per richiamo al pgm di controllo profilo utenti
003200020625      *--------
003300020625     d Tibs34Ds      e ds                  Inz
003400020111
003500020107     D KPJBA         E DS
003600020111
003700020107     D FNLV45DS      E DS
003800021022     D fnlv41ds      E DS
003900020111
004000050216      *------------------------------------------------------------------*
004100050216      *  V A R I A B I L I
004200050216      *------------------------------------------------------------------*
004300050216     d cod             s                   like(tblcod)
004400050216     D key             s                   like(tblkey)
004500050216     D w003a           S              3
004600091022
004700091022     itivis000
004800091022     i              visesi                      wisesi
004900020111
005000020108      *------------------------------------------------------------------*
005100020108      *               RIEPILOGO INDICATORI
005200020108      *------------------------------------------------------------------*
005300020109      * 80    - OVERFLOW
005400020108      *------------------------------------------------------------------*
005500010531      *    T E S T A T A    S T A M P A    T A R I F F A
005600020108      *------------------------------------------------------------------*
005700021022      *
005800050216      * Per errore di reperimento testo vado a fine pgm
005900021022     C                   if        $NoStp   = *on
006000021022     C                   goto      fine
006100021022     C                   endif
006200020108
006300020108      * Recupero il cliente
006400020108     c                   exsr      Sr_Reccli
006500020108
006600020108      * Decodifico i dati del cliente
006700020108
006800020108     c                   exsr      Sr_Decana
006900020108
007000020108      * stampo la prima parte della tariffa
007100020108
007200060905     c                   write     ta62p01
007300020109
007400020109     c     fine          tag
007500050216
007600050216     C                   eval      I69TLA  = 'C'
007700050216     C                   call      'TIBS69R'
007800050216     C                   parm                    TIBS69DS
007900050216
008000020109     c                   seton                                        lr
008100020109
008200020108      *------------------------------------------------------------------*
008300020108      *   Sr_Reccli       RECUPERO CLIENTE
008400020108      *------------------------------------------------------------------*
008500020108
008600020108     c     Sr_Reccli     begsr
008700020108
008800060818      * se è stato passato il numero della visita controllo se devo recuperare
008900060818      * i dati dall'anagrafica provvisoria
009000060818     c                   if        i45nrv <> *zeros
009100091022     c                   select
009200060818      * con la vistita vado su tnvis
009300110103     c**                 when      i45ast = 'V'
009400110103     c**   i45nrv        chain     tnvis05l
009500091022      * con la trattativa vado su tivis
009600091022     c                   when      i45ast = 'T'
009700091022     c     i45nrv        chain     tivis05l
009800091022     c                   endsl
009900060818      * se non esite la visita esco dal programma, ma non dovrebbe capitare
010000091022     c                   if        not %found
010100060818     c                   goto      fine
010200060818     c                   endif
010300060818      * controllo se esiste l'anagrafica provvisoria
010400060818     c     keyaco        chain     tfaco00f
010500060818     c                   if        not %found(tfaco00f)
010600060818      * se non esite l'anagrafica provvisoria e non è una visita di un cliente
010700060818      * codificato esco dal programma, ma non dovrebbe capitare
010800060818     c                   if        visksc = *zeros
010900060818     c                   goto      fine
011000060818     c                   endif
011100060818      * se non esiste l'anagrafica provvisoria ed è una visita di un cliente
011200060818      * codificato recupero i dato dall'anagrafica clienti
011300060818     c                   eval      i45ksc = visksc
011400060818     c                   goto      waco
011500060818     c                   endif
011600060818      * imposto i dati da stampare
011700060818     c     keyaco        chain     tfind00f
011800060818     c                   eval      picrag = acorag
011900060818     c                   eval      picind = indvia
012000060818     c                   eval      piccap = indcae
012100060818     c                   eval      picloc = indcit
012200060818     c                   eval      picpro = indprv
012300060818     c                   endif
012400060818
012500060818     c     waco          tag
012600060818
012700050216     c                   clear                   TIBS69DS
012800060818     c                   move      i45ksc        I69kac
012900060818     c                   move      i45ksc        I69kin
013000060818     c                   move      i45ksc        I69kcp
013100050216     c                   call      'TIBS69R'
013200050216     c                   parm                    TIBS69DS
013300050216     c                   parm                    DS_CNACO
013400050216     c                   parm                    DS_CNIND
013500050216     c                   parm                    DS_CNCLP
013600050216     c                   parm                    DS_FNCLS
013700050216if  2c                   if        O69err <> *on
013800010622      * se non c'è errore valorizzo i campi in stampa
013900010622     c                   eval      picrag = acorag
014000010622     c                   eval      picind = indvia
014100010622     c                   eval      piccap = indcae
014200010622     c                   eval      picloc = indcit
014300010622     c                   eval      picpro = indprv
014400050216     c                   Else
014500050216     C                   goto      fine
014600050216     c                   endif
014700020108
014800020108     c                   endsr
014900020108
015000020108      *------------------------------------------------------------------*
015100020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
015200020108      *------------------------------------------------------------------*
015300020108
015400020108     c     Sr_Decana     begsr
015500020108
015600020108      * decodfico la nazione se diversa da italia
015700020108
015800020108     c                   if        indsta <> *blanks
015900020108     c                   eval      cod ='15'
016000020108     c                   movel     indsta        w003a
016100020108     c                   movel(p)  w003a         key
016200020108     c     tabkey        chain     tabel00f
016300020108     c                   if        %found(tabel00f)
016400020108     c                   movel     tbluni        desnar
016500020108     c                   else
016600020108     c                   clear                   desnar
016700020108     c                   endif
016800020108     c                   endif
016900020108
017000020108     c                   endsr
017100020108
017200020108      *------------------------------------------------------------------*
017300020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
017400020108      *------------------------------------------------------------------*
017500020108
017600020108     c     Sr_Cartab     begsr
017700021022      *
017800021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
017900021022     C                   clear                   Ds1H
018000021022     C                   move      *off          $NoStp            1
018100021022     C                   clear                   Fnlv41ds
018200021022     C                   movel     '3'           I41Mod
018300021022     C                   movel     I45TTX        I41Let
018400021022     C                   CALL      'FNLV41R'
018500021022     C                   PARM                    FNLV41DS
018600021022      *
018700021022      * Se non ho errori stampo
018800021023     C     o41err        ifne      'E'
018900021022     C                   movel     o41dta        DS1H
019000021022     C                   ELSE
019100021022     C                   move      *on           $NoStp
019200021022     C                   ENDIF
019300031120
019400020108     c                   endsr
019500020107
019600020110      *------------------------------------------------------------------*
019700020110      *                   ROUTINE INIZIALE
019800020110      *------------------------------------------------------------------*
019900020108
020000020108     c     *inzsr        begsr
020100020108
020200020108     c     *entry        plist
020300020108     c                   parm                    kpjba
020400020108     c                   parm                    fnlv45ds
020500020108
020600020625     c                   z-add     1             codut             1 0
020700020625
020800020625      * reperisco i dati utente
020900020625     c     *dtaara       define    §azute        AzuteDs
021000020625     c     *dtaara       define    §datiute      DDatiUte
021100020625
021200020625     c                   in(E)     *dtaara
021300020625     c                   IF        %Error or RSUT = *blanks
021400020625     c                   call      'TIBS34R'
021500020625     c                   parm                    Tibs34Ds
021600020625     c                   in        *dtaara
021700020625     c                   ENDIF
021800050216
021900020107      * carico le tabelle
022000020107     c                   exsr      Sr_Cartab
022100070227
022200070227      * apro i file relativi alle visite
022300110103     c**                 if        i45ast = 'V' or i45ast = 'T'
022400110103     c                   if        i45ast = 'T'
022500070227     c                   open      tfaco00f
022600070227     c                   open      tfind00f
022700110103     c**                 if        i45ast = 'V'
022800110103     c**                 open      tnvis05l
022900110103     c**                 endif
023000091022     c                   if        i45ast = 'T'
023100091022     c                   open      tivis05l
023200091022     c                   endif
023300070227     c                   endif
023400030508
023500020108      *------------------------------------------------------------------*
023600020108      *               DEFINIZIONI DELLE CHIAVI
023700020108      *------------------------------------------------------------------*
023800020108
023900020108      * --- TABEL00F ---
024000020108     c     tabkey        klist
024100020108     c                   kfld                    codut
024200020108     c                   kfld                    cod
024300020108     c                   kfld                    key
024400060818
024500060818      * --- TFACO ---
024600060818     c     keyaco        klist
024700060818     c                   kfld                    codut
024800060818     c                   kfld                    dutkci
024900060818     c                   kfld                    i45nrv
025000020108
025100020108     c                   endsr
