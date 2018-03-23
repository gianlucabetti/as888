000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300050216      *        S T A M P A       L E T T E R A
000400060906      *        TESTO 38
000500020108      *------------------------------------------------------------------*
000600020107     FTABEL00F  IF   E           K DISK
000700070227     FTFACO00F  IF   E           K DISK    usropn
000800070227     FTFIND00F  IF   E           K DISK    usropn
000900070227     FTNVIS05L  IF   E           K DISK    usropn
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
004600020111
004700020108      *------------------------------------------------------------------*
004800020108      *               RIEPILOGO INDICATORI
004900020108      *------------------------------------------------------------------*
005000020109      * 80    - OVERFLOW
005100020108      *------------------------------------------------------------------*
005200010531      *    T E S T A T A    S T A M P A    T A R I F F A
005300020108      *------------------------------------------------------------------*
005400021022      *
005500050216      * Per errore di reperimento testo vado a fine pgm
005600021022     C                   if        $NoStp   = *on
005700021022     C                   goto      fine
005800021022     C                   endif
005900020108
006000020108      * Recupero il cliente
006100020108     c                   exsr      Sr_Reccli
006200020108
006300020108      * Decodifico i dati del cliente
006400020108
006500020108     c                   exsr      Sr_Decana
006600020108
006700020108      * stampo la prima parte della tariffa
006800020108
006900060905     c                   write     ta62p01
007000020109
007100020109     c     fine          tag
007200050216
007300050216     C                   eval      I69TLA  = 'C'
007400050216     C                   call      'TIBS69R'
007500050216     C                   parm                    TIBS69DS
007600050216
007700020109     c                   seton                                        lr
007800020109
007900020108      *------------------------------------------------------------------*
008000020108      *   Sr_Reccli       RECUPERO CLIENTE
008100020108      *------------------------------------------------------------------*
008200020108
008300020108     c     Sr_Reccli     begsr
008400020108
008500060818      * se è stato passato il numero della visita controllo se devo recuperare
008600060818      * i dati dall'anagrafica provvisoria
008700060818     c                   if        i45nrv <> *zeros
008800060818      * con la vistita vado su tnvis
008900060818     c     i45nrv        chain     tnvis05l
009000060818      * se non esite la visita esco dal programma, ma non dovrebbe capitare
009100060818     c                   if        not %found(tnvis05l)
009200060818     c                   goto      fine
009300060818     c                   endif
009400060818      * controllo se esiste l'anagrafica provvisoria
009500060818     c     keyaco        chain     tfaco00f
009600060818     c                   if        not %found(tfaco00f)
009700060818      * se non esite l'anagrafica provvisoria e non è una visita di un cliente
009800060818      * codificato esco dal programma, ma non dovrebbe capitare
009900060818     c                   if        visksc = *zeros
010000060818     c                   goto      fine
010100060818     c                   endif
010200060818      * se non esiste l'anagrafica provvisoria ed è una visita di un cliente
010300060818      * codificato recupero i dato dall'anagrafica clienti
010400060818     c                   eval      i45ksc = visksc
010500060818     c                   goto      waco
010600060818     c                   endif
010700060818      * imposto i dati da stampare
010800060818     c     keyaco        chain     tfind00f
010900060818     c                   eval      picrag = acorag
011000060818     c                   eval      picind = indvia
011100060818     c                   eval      piccap = indcae
011200060818     c                   eval      picloc = indcit
011300060818     c                   eval      picpro = indprv
011400060818     c                   endif
011500060818
011600060818     c     waco          tag
011700060818
011800050216     c                   clear                   TIBS69DS
011900060818     c                   move      i45ksc        I69kac
012000060818     c                   move      i45ksc        I69kin
012100060818     c                   move      i45ksc        I69kcp
012200050216     c                   call      'TIBS69R'
012300050216     c                   parm                    TIBS69DS
012400050216     c                   parm                    DS_CNACO
012500050216     c                   parm                    DS_CNIND
012600050216     c                   parm                    DS_CNCLP
012700050216     c                   parm                    DS_FNCLS
012800050216if  2c                   if        O69err <> *on
012900010622      * se non c'è errore valorizzo i campi in stampa
013000010622     c                   eval      picrag = acorag
013100010622     c                   eval      picind = indvia
013200010622     c                   eval      piccap = indcae
013300010622     c                   eval      picloc = indcit
013400010622     c                   eval      picpro = indprv
013500050216     c                   Else
013600050216     C                   goto      fine
013700050216     c                   endif
013800020108
013900020108     c                   endsr
014000020108
014100020108      *------------------------------------------------------------------*
014200020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
014300020108      *------------------------------------------------------------------*
014400020108
014500020108     c     Sr_Decana     begsr
014600020108
014700020108      * decodfico la nazione se diversa da italia
014800020108
014900020108     c                   if        indsta <> *blanks
015000020108     c                   eval      cod ='15'
015100020108     c                   movel     indsta        w003a
015200020108     c                   movel(p)  w003a         key
015300020108     c     tabkey        chain     tabel00f
015400020108     c                   if        %found(tabel00f)
015500020108     c                   movel     tbluni        desnar
015600020108     c                   else
015700020108     c                   clear                   desnar
015800020108     c                   endif
015900020108     c                   endif
016000020108
016100020108     c                   endsr
016200020108
016300020108      *------------------------------------------------------------------*
016400020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
016500020108      *------------------------------------------------------------------*
016600020108
016700020108     c     Sr_Cartab     begsr
016800021022      *
016900021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
017000021022     C                   clear                   Ds1H
017100021022     C                   move      *off          $NoStp            1
017200021022     C                   clear                   Fnlv41ds
017300021022     C                   movel     '3'           I41Mod
017400021022     C                   movel     I45TTX        I41Let
017500021022     C                   CALL      'FNLV41R'
017600021022     C                   PARM                    FNLV41DS
017700021022      *
017800021022      * Se non ho errori stampo
017900021023     C     o41err        ifne      'E'
018000021022     C                   movel     o41dta        DS1H
018100021022     C                   ELSE
018200021022     C                   move      *on           $NoStp
018300021022     C                   ENDIF
018400031120
018500020108     c                   endsr
018600020107
018700020110      *------------------------------------------------------------------*
018800020110      *                   ROUTINE INIZIALE
018900020110      *------------------------------------------------------------------*
019000020108
019100020108     c     *inzsr        begsr
019200020108
019300020108     c     *entry        plist
019400020108     c                   parm                    kpjba
019500020108     c                   parm                    fnlv45ds
019600020108
019700020625     c                   z-add     1             codut             1 0
019800020625
019900020625      * reperisco i dati utente
020000020625     c     *dtaara       define    §azute        AzuteDs
020100020625     c     *dtaara       define    §datiute      DDatiUte
020200020625
020300020625     c                   in(E)     *dtaara
020400020625     c                   IF        %Error or RSUT = *blanks
020500020625     c                   call      'TIBS34R'
020600020625     c                   parm                    Tibs34Ds
020700020625     c                   in        *dtaara
020800020625     c                   ENDIF
020900050216
021000020107      * carico le tabelle
021100020107     c                   exsr      Sr_Cartab
021200070227
021300070227      * apro i file relativi alle visite
021400070227     c                   if        i45ast = 'V'
021500070227     c                   open      tfaco00f
021600070227     c                   open      tfind00f
021700070227     c                   open      tnvis05l
021800070227     c                   endif
021900030508
022000020108      *------------------------------------------------------------------*
022100020108      *               DEFINIZIONI DELLE CHIAVI
022200020108      *------------------------------------------------------------------*
022300020108
022400020108      * --- TABEL00F ---
022500020108     c     tabkey        klist
022600020108     c                   kfld                    codut
022700020108     c                   kfld                    cod
022800020108     c                   kfld                    key
022900060818
023000060818      * --- TFACO ---
023100060818     c     keyaco        klist
023200060818     c                   kfld                    codut
023300060818     c                   kfld                    dutkci
023400060818     c                   kfld                    i45nrv
023500020108
023600020108     c                   endsr
