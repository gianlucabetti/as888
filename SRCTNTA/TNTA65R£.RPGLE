000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300050216      *        S T A M P A       L E T T E R A
000400060906      *        TESTO 38
000500020108      *------------------------------------------------------------------*
000600020107     FTABEL00F  IF   E           K DISK
000700070227     FTFACO00F  IF   E           K DISK    usropn
000800070227     FTFIND00F  IF   E           K DISK    usropn
000900070227     FTNVIS05L  IF   E           K DISK    usropn
001000091022     fTivis05l  if   e           k disk    usropn
001100060906     FTNTA65P   O    E             PRINTER oflind(*in80)
001200020108
001300020108      *------------------------------------------------------------------*
001400050216      *  D S   E S T E R N E
001500020108      *------------------------------------------------------------------*
001600050216     d TIBS69DS      e ds                  inz
001700050216     d ds_CNACO      e ds                  inz  extname(CNACO00F)
001800050216     d ds_CNIND      e ds                  inz  extname(CNIND00F)
001900050216     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
002000050216     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
002100020107      * DS DEL FILE TABEL00F
002200050216     D DS1H          E DS
002300020625      *--------
002400020625      * Ds di riferimento al file esterno AzUte00F
002500020625      *--------
002600020625     d AzuteDs       e ds                  ExtName(AzUte00F)
002700020625      *--------
002800020625      * Ds per dati organigramma
002900020625      *--------
003000020625     d DDatiUte      e ds
003100020625      *--------
003200020625      * Parametri per richiamo al pgm di controllo profilo utenti
003300020625      *--------
003400020625     d Tibs34Ds      e ds                  Inz
003500020111
003600020107     D KPJBA         E DS
003700020111
003800020107     D FNLV45DS      E DS
003900021022     D fnlv41ds      E DS
004000020111
004100050216      *------------------------------------------------------------------*
004200050216      *  V A R I A B I L I
004300050216      *------------------------------------------------------------------*
004400050216     d cod             s                   like(tblcod)
004500050216     D key             s                   like(tblkey)
004600050216     D w003a           S              3
004700091022
004800091022     itivis000
004900091022     i              visesi                      wisesi
005000020111
005100020108      *------------------------------------------------------------------*
005200020108      *               RIEPILOGO INDICATORI
005300020108      *------------------------------------------------------------------*
005400020109      * 80    - OVERFLOW
005500020108      *------------------------------------------------------------------*
005600010531      *    T E S T A T A    S T A M P A    T A R I F F A
005700020108      *------------------------------------------------------------------*
005800021022      *
005900050216      * Per errore di reperimento testo vado a fine pgm
006000021022     C                   if        $NoStp   = *on
006100021022     C                   goto      fine
006200021022     C                   endif
006300020108
006400020108      * Recupero il cliente
006500020108     c                   exsr      Sr_Reccli
006600020108
006700020108      * Decodifico i dati del cliente
006800020108
006900020108     c                   exsr      Sr_Decana
007000020108
007100020108      * stampo la prima parte della tariffa
007200020108
007300060905     c                   write     ta62p01
007400020109
007500020109     c     fine          tag
007600050216
007700050216     C                   eval      I69TLA  = 'C'
007800050216     C                   call      'TIBS69R'
007900050216     C                   parm                    TIBS69DS
008000050216
008100020109     c                   seton                                        lr
008200020109
008300020108      *------------------------------------------------------------------*
008400020108      *   Sr_Reccli       RECUPERO CLIENTE
008500020108      *------------------------------------------------------------------*
008600020108
008700020108     c     Sr_Reccli     begsr
008800020108
008900060818      * se è stato passato il numero della visita controllo se devo recuperare
009000060818      * i dati dall'anagrafica provvisoria
009100060818     c                   if        i45nrv <> *zeros
009200091022     c                   select
009300060818      * con la vistita vado su tnvis
009400091022     c                   when      i45ast = 'V'
009500060818     c     i45nrv        chain     tnvis05l
009600091022      * con la trattativa vado su tivis
009700091022     c                   when      i45ast = 'T'
009800091022     c     i45nrv        chain     tivis05l
009900091022     c                   endsl
010000060818      * se non esite la visita esco dal programma, ma non dovrebbe capitare
010100091022     c                   if        not %found
010200060818     c                   goto      fine
010300060818     c                   endif
010400060818      * controllo se esiste l'anagrafica provvisoria
010500060818     c     keyaco        chain     tfaco00f
010600060818     c                   if        not %found(tfaco00f)
010700060818      * se non esite l'anagrafica provvisoria e non è una visita di un cliente
010800060818      * codificato esco dal programma, ma non dovrebbe capitare
010900060818     c                   if        visksc = *zeros
011000060818     c                   goto      fine
011100060818     c                   endif
011200060818      * se non esiste l'anagrafica provvisoria ed è una visita di un cliente
011300060818      * codificato recupero i dato dall'anagrafica clienti
011400060818     c                   eval      i45ksc = visksc
011500060818     c                   goto      waco
011600060818     c                   endif
011700060818      * imposto i dati da stampare
011800060818     c     keyaco        chain     tfind00f
011900060818     c                   eval      picrag = acorag
012000060818     c                   eval      picind = indvia
012100060818     c                   eval      piccap = indcae
012200060818     c                   eval      picloc = indcit
012300060818     c                   eval      picpro = indprv
012400060818     c                   endif
012500060818
012600060818     c     waco          tag
012700060818
012800050216     c                   clear                   TIBS69DS
012900060818     c                   move      i45ksc        I69kac
013000060818     c                   move      i45ksc        I69kin
013100060818     c                   move      i45ksc        I69kcp
013200050216     c                   call      'TIBS69R'
013300050216     c                   parm                    TIBS69DS
013400050216     c                   parm                    DS_CNACO
013500050216     c                   parm                    DS_CNIND
013600050216     c                   parm                    DS_CNCLP
013700050216     c                   parm                    DS_FNCLS
013800050216if  2c                   if        O69err <> *on
013900010622      * se non c'è errore valorizzo i campi in stampa
014000010622     c                   eval      picrag = acorag
014100010622     c                   eval      picind = indvia
014200010622     c                   eval      piccap = indcae
014300010622     c                   eval      picloc = indcit
014400010622     c                   eval      picpro = indprv
014500050216     c                   Else
014600050216     C                   goto      fine
014700050216     c                   endif
014800020108
014900020108     c                   endsr
015000020108
015100020108      *------------------------------------------------------------------*
015200020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
015300020108      *------------------------------------------------------------------*
015400020108
015500020108     c     Sr_Decana     begsr
015600020108
015700020108      * decodfico la nazione se diversa da italia
015800020108
015900020108     c                   if        indsta <> *blanks
016000020108     c                   eval      cod ='15'
016100020108     c                   movel     indsta        w003a
016200020108     c                   movel(p)  w003a         key
016300020108     c     tabkey        chain     tabel00f
016400020108     c                   if        %found(tabel00f)
016500020108     c                   movel     tbluni        desnar
016600020108     c                   else
016700020108     c                   clear                   desnar
016800020108     c                   endif
016900020108     c                   endif
017000020108
017100020108     c                   endsr
017200020108
017300020108      *------------------------------------------------------------------*
017400020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
017500020108      *------------------------------------------------------------------*
017600020108
017700020108     c     Sr_Cartab     begsr
017800021022      *
017900021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
018000021022     C                   clear                   Ds1H
018100021022     C                   move      *off          $NoStp            1
018200021022     C                   clear                   Fnlv41ds
018300021022     C                   movel     '3'           I41Mod
018400021022     C                   movel     I45TTX        I41Let
018500021022     C                   CALL      'FNLV41R'
018600021022     C                   PARM                    FNLV41DS
018700021022      *
018800021022      * Se non ho errori stampo
018900021023     C     o41err        ifne      'E'
019000021022     C                   movel     o41dta        DS1H
019100021022     C                   ELSE
019200021022     C                   move      *on           $NoStp
019300021022     C                   ENDIF
019400031120
019500020108     c                   endsr
019600020107
019700020110      *------------------------------------------------------------------*
019800020110      *                   ROUTINE INIZIALE
019900020110      *------------------------------------------------------------------*
020000020108
020100020108     c     *inzsr        begsr
020200020108
020300020108     c     *entry        plist
020400020108     c                   parm                    kpjba
020500020108     c                   parm                    fnlv45ds
020600020108
020700020625     c                   z-add     1             codut             1 0
020800020625
020900020625      * reperisco i dati utente
021000020625     c     *dtaara       define    §azute        AzuteDs
021100020625     c     *dtaara       define    §datiute      DDatiUte
021200020625
021300020625     c                   in(E)     *dtaara
021400020625     c                   IF        %Error or RSUT = *blanks
021500020625     c                   call      'TIBS34R'
021600020625     c                   parm                    Tibs34Ds
021700020625     c                   in        *dtaara
021800020625     c                   ENDIF
021900050216
022000020107      * carico le tabelle
022100020107     c                   exsr      Sr_Cartab
022200070227
022300070227      * apro i file relativi alle visite
022400091022     c                   if        i45ast = 'V' or i45ast = 'T'
022500070227     c                   open      tfaco00f
022600070227     c                   open      tfind00f
022700091022     c                   if        i45ast = 'V'
022800070227     c                   open      tnvis05l
022900091022     c                   endif
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
