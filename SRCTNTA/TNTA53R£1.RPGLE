000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300050216      *        S T A M P A       L E T T E R A
000400050216      *        TESTO 40
000500020108      *------------------------------------------------------------------*
000600050216     FTFNTC01L  IF   E           K DISK
000700020107     FAZORG01L  IF   E           K DISK
000800020107     FTABEL00F  IF   E           K DISK
000900050216     FTNTA52P   O    E             PRINTER oflind(*in80)
001000020108
001100020108      *------------------------------------------------------------------*
001200050216      *  D S   E S T E R N E
001300020108      *------------------------------------------------------------------*
001400050216     d TIBS69DS      e ds                  inz
001500050216     d ds_CNACO      e ds                  inz  extname(CNACO00F)
001600050216     d ds_CNIND      e ds                  inz  extname(CNIND00F)
001700050216     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
001800050216     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
001900020107      * DS DEL FILE TABEL00F
002000050216     D DS1H          E DS
002100050216      * DS organigramma
002200020107     D OG143         E DS
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
003500020625
003600020111
003700020107     D KPJBA         E DS
003800020107     D  utente                 1      5
003900020111
004000020107     D FNLV45DS      E DS
004100021022     D fnlv41ds      E DS
004200020111
004300050216      *------------------------------------------------------------------*
004400050216      *  V A R I A B I L I
004500050216      *------------------------------------------------------------------*
004600050216     d cod             s                   like(tblcod)
004700050216     D comfil          s              3  0
004800020107     D DATA_eur        S               D   DATFMT(*eur)
004900050216     D datastampa      s             10
005000050216     D key             s                   like(tblkey)
005100050216     D notapl          s                   like(ntcapl)
005200050216     D notke1          s                   like(ntcnk1)
005300050216     D notke2          s                   like(ntcnk2)
005400050216     D nottnt          s                   like(ntctnt)
005500050216     D w003a           S              3
005600020111
005700020108      *------------------------------------------------------------------*
005800020108      *               RIEPILOGO INDICATORI
005900020108      *------------------------------------------------------------------*
006000050216      * 70    - CONDIZIONA LA STAMPA DEI RIFERIMENTI
006100050216      * 73    - CONDIZIONA LA STAMPA DEL RESPONSABILE TRASPORTI
006200020108      * 77    - CONDIZIONA LA STAMPA DEL E.P.C.
006300020109      * 80    - OVERFLOW
006400020108      *------------------------------------------------------------------*
006500010531      *    T E S T A T A    S T A M P A    T A R I F F A
006600020108      *------------------------------------------------------------------*
006700021022      *
006800050216      * Per errore di reperimento testo vado a fine pgm
006900021022     C                   if        $NoStp   = *on
007000021022     C                   goto      fine
007100021022     C                   endif
007200020108
007300020108      * Recupero il cliente
007400020108     c                   exsr      Sr_Reccli
007500020108
007600020108      * Decodifico i dati del cliente
007700020108
007800020108     c                   exsr      Sr_Decana
007900050216      * Recupero note per responsabile trasporti
008000050216
008100050216     c                   exsr      Sr_Recnot
008200020108
008300020108      * Decodifico i dati del punto operativo
008400020108
008500020108     c                   exsr      Sr_Decpo
008600020108
008700020108      * stampo la prima parte della tariffa
008800020108
008900050216     c                   write     ta52pPP
009000050216     c                   write     ta52p01
009100020109
009200020109     c     fine          tag
009300050216
009400050216     C                   eval      I69TLA  = 'C'
009500050216     C                   call      'TIBS69R'
009600050216     C                   parm                    TIBS69DS
009700050216
009800020109     c                   seton                                        lr
009900020109
010000020108      *------------------------------------------------------------------*
010100020108      *   Sr_Reccli       RECUPERO CLIENTE
010200020108      *------------------------------------------------------------------*
010300020108
010400020108     c     Sr_Reccli     begsr
010500020108
010600050216     c                   clear                   TIBS69DS
010700050216     c                   move      parsks        I69kac
010800050216     c                   move      parsks        I69kin
010900050216     c                   move      parsks        I69kcp
011000050216     c                   call      'TIBS69R'
011100050216     c                   parm                    TIBS69DS
011200050216     c                   parm                    DS_CNACO
011300050216     c                   parm                    DS_CNIND
011400050216     c                   parm                    DS_CNCLP
011500050216     c                   parm                    DS_FNCLS
011600050216if  2c                   if        O69err <> *on
011700010622      * se non c'è errore valorizzo i campi in stampa
011800010622     c                   eval      picrag = acorag
011900010622     c                   eval      picind = indvia
012000010622     c                   eval      piccap = indcae
012100010622     c                   eval      picloc = indcit
012200010622     c                   eval      picpro = indprv
012300050216     c                   Else
012400050216     C                   goto      fine
012500050216     c                   endif
012600020108
012700020108     c                   endsr
012800020108
012900020108      *------------------------------------------------------------------*
013000020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
013100020108      *------------------------------------------------------------------*
013200020108
013300020108     c     Sr_Decana     begsr
013400050216
013500050216     c                   Eval      *In70 = *Off
013600050216
013700050216      * valorizzo i numeri riferimento
013800050216     c                   eval      picnr1 = i45nr1
013900050216     c                   eval      picnr2 = i45nr2
014000050216     c                   If        picnr1 <> *Blanks or picnr2 <> *Blanks
014100050216     c                   Eval      *In70 = *On
014200050216     c                   EndIf
014300020108
014400020108      * decodfico la nazione se diversa da italia
014500020108
014600020108     c                   if        indsta <> *blanks
014700020108     c                   eval      cod ='15'
014800020108     c                   movel     indsta        w003a
014900020108     c                   movel(p)  w003a         key
015000020108     c     tabkey        chain     tabel00f
015100020108     c                   if        %found(tabel00f)
015200020108     c                   movel     tbluni        desnar
015300020108     c                   else
015400020108     c                   clear                   desnar
015500020108     c                   endif
015600020108
015700020108     c                   endif
015800020108
015900020108      * decodifico il commerciale
016000020108     c                   clear                   key
016100020108     c                   if        clpage > 0
016200020108     c                   movel(p)  clpage        key
016300020108     c                   endif
016400020108
016500020108     c                   if        key <> *blanks
016600020108
016700020108     c                   eval      cod = '01'
016800020108     c     tabkey        chain     tabel00f
016900020108     c                   if        %found(tabel00f)
017000020108     c                   movel     tbluni        descom
017100020108     c                   else
017200020108     c                   clear                   descom
017300020108     c                   endif
017400020108
017500020108     c                   endif
017600020108
017700020108     c                   endsr
017800020108
017900050216      *------------------------------------------------------------------*
018000050216      *   Sr_Recnot       RECUPERA NOTE CLIENTI
018100050216      *------------------------------------------------------------------*
018200050216
018300050216     c     Sr_Recnot     begsr
018400050216
018500050216     c                   eval      *in73 = *Off
018600050216     c                   eval      *in77 = *Off
018700050216
018800050216      * chiave note tariffe/offerta cliente
018900050216
019000050216     c                   eval      notapl = 'C'
019100050216
019200050216      * Responsabile trasporti
019300050216     c                   movel     '05'          nottnt
019400050216     c                   movel     i45kcc        notke1
019500050216     c                   move      parsks        notke1
019600091113     c     keyntc        setll     tfntc01l
019700050216     c                   do        *hival
019800050216     c     keyntc        reade     tfntc01l
019900050216
020000050216     c                   if        %eof(tfntc01l)
020100050216     c                   leave
020200050216     c                   endif
020300050216      * se annullato oppure è una nota interna leggo la successiva
020400050216     c                   if        ntcflt = 'A' or ntcsns <> 'S'
020500050216     c                   iter
020600050216     c                   endif
020700050216      * se alla c.a. è uguale a blank carico la nota
020800050216     c                   if        resptra = *blanks
020900050216     c                   movel     ntcrnt        resptra
021000050216     c                   seton                                        73
021100050216      *
021200050216     c                   else
021300050216      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
021400050216     c                   movel     ntcrnt        resptra2
021500050216     c                   seton                                        77
021600050216     c                   leave
021700050216     c                   endif
021800050216
021900050216     c                   enddo
022000050216
022100050216     c                   endsr
022200050216
022300020108      *------------------------------------------------------------------*
022400020108      *   Sr_Decpo        DECODIFICA I DATI DEL PUNTO OPERATIVO
022500020108      *------------------------------------------------------------------*
022600020108
022700020108     c     Sr_Decpo      begsr
022800020108
022900050216      * recupero il p.o. appartenenza
023000020108
023100020108     c                   movel     parsks        comfil
023200020108
023300020108      * decodifica
023400020108
023500020108     c     comfil        chain     azorg01l
023600020108
023700020108     c                   if        %found(azorg01l)
023800020108     c                   movel     orgdes        td1fil
023900050216     c                   movel     orgdes        pd1fil
024000020108     c                   movel     orgind        td1via
024100020108     c                   movel     orgcpf        td1cap
024200020108     c                   movel     orgloc        td1loc
024300020108     c                   movel     orgpro        td1prv
024400020108     c                   movel     orgtel        td2tel
024500020108     c                   movel     orgfax        td2tfx
024600020108     c                   movel     orgde3        og143
024700020108     c                   endif
024800020108
024900020108      * costruisco la data stampa
025000020108     c                   movel     i45gds        data_eur
025100020108     c     *eur          movel     data_eur      datastampa
025200060613      * imposto nelle varie lingue
025300060613     c                   select
025400060613     c                   when      i45lin = 'I'
025500020108     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
025600060613     c                   when      i45lin = '2'
025700060613     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
025800060818     c                   when      i45lin = '3'
025900060818     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
026000060818     c                   when      i45lin = '4'
026100060818     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
026200060613     c                   endsl
026300020108
026400020108     c                   endsr
026500020108
026600020108      *------------------------------------------------------------------*
026700020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
026800020108      *------------------------------------------------------------------*
026900020108
027000020108     c     Sr_Cartab     begsr
027100021022      *
027200021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
027300021022     C                   clear                   Ds1H
027400021022     C                   move      *off          $NoStp            1
027500021022     C                   clear                   Fnlv41ds
027600021022     C                   movel     '3'           I41Mod
027700021022     C                   movel     I45TTX        I41Let
027800021022     C                   CALL      'FNLV41R'
027900021022     C                   PARM                    FNLV41DS
028000021022      *
028100021022      * Se non ho errori stampo
028200021023     C     o41err        ifne      'E'
028300021022     C                   movel     o41dta        DS1H
028400021022     C                   ELSE
028500021022     C                   move      *on           $NoStp
028600021022     C                   ENDIF
028700031120
028800020107
028900020108     c                   endsr
029000020107
029100020110      *------------------------------------------------------------------*
029200020110      *                   ROUTINE INIZIALE
029300020110      *------------------------------------------------------------------*
029400020108
029500020108     c     *inzsr        begsr
029600020108
029700020108     c     *entry        plist
029800020108     c                   parm                    kpjba
029900020108     c                   parm                    fnlv45ds
030000020108
030100020625     c                   z-add     1             codut             1 0
030200020625
030300020625      * reperisco i dati utente
030400020625     c     *dtaara       define    §azute        AzuteDs
030500020625     c     *dtaara       define    §datiute      DDatiUte
030600020625
030700020625     c                   in(E)     *dtaara
030800020625     c                   IF        %Error or RSUT = *blanks
030900020625     c                   call      'TIBS34R'
031000020625     c                   parm                    Tibs34Ds
031100020625     c                   in        *dtaara
031200020625     c                   ENDIF
031300050216
031400020107      * carico le tabelle
031500020107     c                   exsr      Sr_Cartab
031600021022      *
031700021022      * proseguo se ho i dati per uso testi
031800021022     c                   if        $NoStp = *off
031900020108
032000020108      * valorizzo il campo del codice cliente tariffa
032100020108     c                   eval      parsks = i45ksc
032200020108
032300021022     c                   endif
032400030508
032500030508
032600020108      *------------------------------------------------------------------*
032700020108      *               DEFINIZIONI DELLE CHIAVI
032800020108      *------------------------------------------------------------------*
032900020108
033000020108      * --- TABEL00F ---
033100020108     c     tabkey        klist
033200020108     c                   kfld                    codut
033300020108     c                   kfld                    cod
033400020108     c                   kfld                    key
033500050216
033600050216      * --- TFNTC ---
033700050216     c     keyntc        klist
033800050216     c                   kfld                    notapl
033900050216     c                   kfld                    notke1
034000050216     c                   kfld                    notke2
034100050216     c                   kfld                    nottnt
034200020108
034300020108     c                   endsr
