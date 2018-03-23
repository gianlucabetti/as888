000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300050216      *        S T A M P A       L E T T E R A
000400060818      *        TESTO 05
000500020108      *------------------------------------------------------------------*
000600050216     FTFNTC01L  IF   E           K DISK
000700020107     FAZORG01L  IF   E           K DISK
000800020107     FTABEL00F  IF   E           K DISK
000900070227     FTFACO00F  IF   E           K DISK    usropn
001000070227     FTFIND00F  IF   E           K DISK    usropn
001100070227     FTFCLP00F  IF   E           K DISK    usropn
001200091022     fTivis05l  if   e           k disk    usropn
001300060818     FTNTA54P   O    E             PRINTER oflind(*in80)
001400020108
001500020108      *------------------------------------------------------------------*
001600050216      *  D S   E S T E R N E
001700020108      *------------------------------------------------------------------*
001800050216     d TIBS69DS      e ds                  inz
001900050216     d ds_CNACO      e ds                  inz  extname(CNACO00F)
002000050216     d ds_CNIND      e ds                  inz  extname(CNIND00F)
002100050216     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
002200050216     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
002300020107      * DS DEL FILE TABEL00F
002400050216     D DS1H          E DS
002500050216      * DS organigramma
002600020107     D OG143         E DS
002700020625      *--------
002800020625      * Ds di riferimento al file esterno AzUte00F
002900020625      *--------
003000020625     d AzuteDs       e ds                  ExtName(AzUte00F)
003100020625      *--------
003200020625      * Ds per dati organigramma
003300020625      *--------
003400020625     d DDatiUte      e ds
003500020625      *--------
003600020625      * Parametri per richiamo al pgm di controllo profilo utenti
003700020625      *--------
003800020625     d Tibs34Ds      e ds                  Inz
003900020625
004000020111
004100020107     D KPJBA         E DS
004200020107     D  utente                 1      5
004300020111
004400020107     D FNLV45DS      E DS
004500021022     D fnlv41ds      E DS
004600020111
004700050216      *------------------------------------------------------------------*
004800050216      *  V A R I A B I L I
004900050216      *------------------------------------------------------------------*
005000050216     d cod             s                   like(tblcod)
005100050216     D comfil          s              3  0
005200020107     D DATA_eur        S               D   DATFMT(*eur)
005300050216     D datastampa      s             10
005400050216     D key             s                   like(tblkey)
005500050216     D notapl          s                   like(ntcapl)
005600050216     D notke1          s                   like(ntcnk1)
005700050216     D notke2          s                   like(ntcnk2)
005800050216     D nottnt          s                   like(ntctnt)
005900050216     D w003a           S              3
006000091113     d $trattativa     s              1n
006100091022
006200091022     itivis000
006300091022     i              visesi                      wisesi
006400020111
006500020108      *------------------------------------------------------------------*
006600020108      *               RIEPILOGO INDICATORI
006700020108      *------------------------------------------------------------------*
006800050216      * 70    - CONDIZIONA LA STAMPA DEI RIFERIMENTI
006900050216      * 73    - CONDIZIONA LA STAMPA DEL RESPONSABILE TRASPORTI
007000020108      * 77    - CONDIZIONA LA STAMPA DEL E.P.C.
007100020109      * 80    - OVERFLOW
007200020108      *------------------------------------------------------------------*
007300010531      *    T E S T A T A    S T A M P A    T A R I F F A
007400020108      *------------------------------------------------------------------*
007500021022      *
007600050216      * Per errore di reperimento testo vado a fine pgm
007700021022     C                   if        $NoStp   = *on
007800021022     C                   goto      fine
007900021022     C                   endif
008000020108
008100020108      * Recupero il cliente
008200020108     c                   exsr      Sr_Reccli
008300020108
008400020108      * Decodifico i dati del cliente
008500020108
008600020108     c                   exsr      Sr_Decana
008700050216      * Recupero note per responsabile trasporti
008800050216
008900050216     c                   exsr      Sr_Recnot
009000020108
009100020108      * Decodifico i dati del punto operativo
009200020108
009300020108     c                   exsr      Sr_Decpo
009400020108
009500020108      * stampo la prima parte della tariffa
009600020108
009700060818     c                   write     ta54pPP
009800060818     c                   write     ta54p01
009900020109
010000020109     c     fine          tag
010100050216
010200050216     C                   eval      I69TLA  = 'C'
010300050216     C                   call      'TIBS69R'
010400050216     C                   parm                    TIBS69DS
010500050216
010600020109     c                   seton                                        lr
010700020109
010800020108      *------------------------------------------------------------------*
010900020108      *   Sr_Reccli       RECUPERO CLIENTE
011000020108      *------------------------------------------------------------------*
011100020108
011200020108     c     Sr_Reccli     begsr
011300091113
011400091211     c                   eval      $trattativa = *off
011500020108
011600060818      * se è stato passato il numero della visita controllo se devo recuperare
011700060818      * i dati dall'anagrafica provvisoria
011800060818     c                   if        i45nrv <> *zeros
011900091022     c                   select
012000060818      * con la vistita vado su tnvis
012100110103     c**                 when      i45ast = 'V'
012200110103     c**   i45nrv        chain     tnvis05l
012300091022      * con la trattativa vado su tivis
012400091022     c                   when      i45ast = 'T'
012500091113     c                   eval      $trattativa = *on
012600091022     c     i45nrv        chain     tivis05l
012700091022     c                   endsl
012800060818      * se non esite la visita esco dal programma, ma non dovrebbe capitare
012900091022     c                   if        not %found
013000060818     c                   goto      fine
013100060818     c                   endif
013200060818      * controllo se esiste l'anagrafica provvisoria
013300060818     c     keyaco        chain     tfaco00f
013400060818     c                   if        not %found(tfaco00f)
013500060818      * se non esite l'anagrafica provvisoria e non è una visita di un cliente
013600060818      * codificato esco dal programma, ma non dovrebbe capitare
013700060818     c                   if        visksc = *zeros
013800060818     c                   goto      fine
013900060818     c                   endif
014000060818      * se non esiste l'anagrafica provvisoria ed è una visita di un cliente
014100060818      * codificato recupero i dato dall'anagrafica clienti
014200060818     c                   eval      i45ksc = visksc
014300060818     c                   goto      waco
014400060818     c                   endif
014500060818      * imposto i dati da stampare
014600060818     c     keyaco        chain     tfind00f
014700060818     c                   eval      picrag = acorag
014800060818     c                   eval      picind = indvia
014900060818     c                   eval      piccap = indcae
015000060818     c                   eval      picloc = indcit
015100060818     c                   eval      picpro = indprv
015200060818     c                   endif
015300060818
015400060818     c     waco          tag
015500060818
015600050216     c                   clear                   TIBS69DS
015700060818     c                   move      i45ksc        I69kac
015800060818     c                   move      i45ksc        I69kin
015900060818     c                   move      i45ksc        I69kcp
016000050216     c                   call      'TIBS69R'
016100050216     c                   parm                    TIBS69DS
016200050216     c                   parm                    DS_CNACO
016300050216     c                   parm                    DS_CNIND
016400050216     c                   parm                    DS_CNCLP
016500050216     c                   parm                    DS_FNCLS
016600050216if  2c                   if        O69err <> *on
016700010622      * se non c'è errore valorizzo i campi in stampa
016800010622     c                   eval      picrag = acorag
016900010622     c                   eval      picind = indvia
017000010622     c                   eval      piccap = indcae
017100010622     c                   eval      picloc = indcit
017200010622     c                   eval      picpro = indprv
017300050216     c                   Else
017400050216     C                   goto      fine
017500050216     c                   endif
017600020108
017700020108     c                   endsr
017800020108
017900020108      *------------------------------------------------------------------*
018000020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
018100020108      *------------------------------------------------------------------*
018200020108
018300020108     c     Sr_Decana     begsr
018400050216
018500050216     c                   Eval      *In70 = *Off
018600050216
018700050216      * valorizzo i numeri riferimento
018800050216     c                   eval      picnr1 = i45nr1
018900050216     c                   eval      picnr2 = i45nr2
019000050216     c                   If        picnr1 <> *Blanks or picnr2 <> *Blanks
019100050216     c                   Eval      *In70 = *On
019200050216     c                   EndIf
019300020108
019400020108      * decodfico la nazione se diversa da italia
019500020108
019600020108     c                   if        indsta <> *blanks
019700020108     c                   eval      cod ='15'
019800020108     c                   movel     indsta        w003a
019900020108     c                   movel(p)  w003a         key
020000020108     c     tabkey        chain     tabel00f
020100020108     c                   if        %found(tabel00f)
020200020108     c                   movel     tbluni        desnar
020300020108     c                   else
020400020108     c                   clear                   desnar
020500020108     c                   endif
020600020108
020700020108     c                   endif
020800020108
020900020108      * decodifico il commerciale
021000020108     c                   clear                   key
021100060818      * se è un cliente lo recupero dall'anagrafica
021200060818     c                   if        i45ksc > *zeros
021300060818     c                   if        clpage = 0
021400060818     c                   leavesr
021500060818     c                   endif
021600020108     c                   movel(p)  clpage        key
021700060818      * se è una visita lo recupero dalla visita
021800060818     c                   else
021900060818     c                   if        viscmm = 0
022000060818     c                   leavesr
022100060818     c                   endif
022200060818     c                   movel(p)  viscmm        key
022300060818     c                   endif
022400020108
022500020108     c                   eval      cod = '01'
022600020108     c     tabkey        chain     tabel00f
022700020108     c                   if        %found(tabel00f)
022800020108     c                   movel     tbluni        descom
022900020108     c                   else
023000020108     c                   clear                   descom
023100020108     c                   endif
023200020108
023300020108     c                   endsr
023400020108
023500050216      *------------------------------------------------------------------*
023600050216      *   Sr_Recnot       RECUPERA NOTE CLIENTI
023700050216      *------------------------------------------------------------------*
023800050216
023900050216     c     Sr_Recnot     begsr
024000050216
024100050216     c                   eval      *in73 = *Off
024200050216     c                   eval      *in77 = *Off
024300050216
024400050216      * chiave note tariffe/offerta cliente
024500060818     c                   movel     i45kcc        notke1
024600060818      * se cliente
024700060818     c                   if        i45ksc <> *zeros
024800050216     c                   eval      notapl = 'C'
024900060818     c                   move      i45ksc        notke1
025000091113      * se visita/trattativa
025100060818     c                   else
025200091113     c                   eval      notapl = i45ast
025300060818     c                   move      i45nrv        notke1
025400091113      * se trattativa su cliente cerco con il cliente
025500091113     c                   if        $trattativa and visksc > 0
025600091113     c                   eval      notapl = 'C'
025700091113     c                   move      visksc        notke1
025800091113     c                   endif
025900060818     c                   endif
026000050216
026100050216      * Responsabile trasporti
026200050216     c                   movel     '05'          nottnt
026300050216      * cerco prima con in chiave il codice tariffa
026400050216     c     keyntc        setll     tfntc01l
026500050216     c                   do        *hival
026600050216     c     keyntc        reade     tfntc01l
026700050216
026800050216     c                   if        %eof(tfntc01l)
026900050216     c                   leave
027000050216     c                   endif
027100050216      * se annullato oppure è una nota interna leggo la successiva
027200050216     c                   if        ntcflt = 'A' or ntcsns <> 'S'
027300050216     c                   iter
027400050216     c                   endif
027500050216      * se alla c.a. è uguale a blank carico la nota
027600050216     c                   if        resptra = *blanks
027700050216     c                   movel     ntcrnt        resptra
027800050216     c                   seton                                        73
027900050216      *
028000050216     c                   else
028100050216      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
028200050216     c                   movel     ntcrnt        resptra2
028300050216     c                   seton                                        77
028400050216     c                   leave
028500050216     c                   endif
028600050216
028700050216     c                   enddo
028800050216
028900050216     c                   endsr
029000050216
029100020108      *------------------------------------------------------------------*
029200020108      *   Sr_Decpo        DECODIFICA I DATI DEL PUNTO OPERATIVO
029300020108      *------------------------------------------------------------------*
029400020108
029500020108     c     Sr_Decpo      begsr
029600020108
029700050216      * recupero il p.o. appartenenza
029800020108
029900060818     c                   if        i45ksc <> *zeros
030000060818     c                   movel     i45ksc        comfil
030100060818     c                   else
030200060818     c                   movel     viscmm        comfil
030300060818     c                   endif
030400020108
030500020108      * decodifica
030600020108
030700020108     c     comfil        chain     azorg01l
030800020108
030900020108     c                   if        %found(azorg01l)
031000020108     c                   movel     orgdes        td1fil
031100050216     c                   movel     orgdes        pd1fil
031200020108     c                   movel     orgind        td1via
031300020108     c                   movel     orgcpf        td1cap
031400020108     c                   movel     orgloc        td1loc
031500020108     c                   movel     orgpro        td1prv
031600020108     c                   movel     orgtel        td2tel
031700020108     c                   movel     orgfax        td2tfx
031800020108     c                   movel     orgde3        og143
031900020108     c                   endif
032000020108
032100020108      * costruisco la data stampa
032200020108     c                   movel     i45gds        data_eur
032300020108     c     *eur          movel     data_eur      datastampa
032400060613      * imposto nelle varie lingue
032500060613     c                   select
032600060613     c                   when      i45lin = 'I'
032700020108     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
032800060613     c                   when      i45lin = '2'
032900060613     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
033000060818     c                   when      i45lin = '3'
033100060818     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
033200060818     c                   when      i45lin = '4'
033300060818     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
033400060613     c                   endsl
033500020108
033600020108     c                   endsr
033700020108
033800020108      *------------------------------------------------------------------*
033900020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
034000020108      *------------------------------------------------------------------*
034100020108
034200020108     c     Sr_Cartab     begsr
034300021022      *
034400021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
034500021022     C                   clear                   Ds1H
034600021022     C                   move      *off          $NoStp            1
034700021022     C                   clear                   Fnlv41ds
034800021022     C                   movel     '3'           I41Mod
034900021022     C                   movel     I45TTX        I41Let
035000021022     C                   CALL      'FNLV41R'
035100021022     C                   PARM                    FNLV41DS
035200021022      *
035300021022      * Se non ho errori stampo
035400021023     C     o41err        ifne      'E'
035500021022     C                   movel     o41dta        DS1H
035600021022     C                   ELSE
035700021022     C                   move      *on           $NoStp
035800021022     C                   ENDIF
035900031120
036000020108     c                   endsr
036100020107
036200020110      *------------------------------------------------------------------*
036300020110      *                   ROUTINE INIZIALE
036400020110      *------------------------------------------------------------------*
036500020108
036600020108     c     *inzsr        begsr
036700020108
036800020108     c     *entry        plist
036900020108     c                   parm                    kpjba
037000020108     c                   parm                    fnlv45ds
037100020108
037200020625     c                   z-add     1             codut             1 0
037300020625
037400020625      * reperisco i dati utente
037500020625     c     *dtaara       define    §azute        AzuteDs
037600020625     c     *dtaara       define    §datiute      DDatiUte
037700020625
037800020625     c                   in(E)     *dtaara
037900020625     c                   IF        %Error or RSUT = *blanks
038000020625     c                   call      'TIBS34R'
038100020625     c                   parm                    Tibs34Ds
038200020625     c                   in        *dtaara
038300020625     c                   ENDIF
038400050216
038500020107      * carico le tabelle
038600020107     c                   exsr      Sr_Cartab
038700070227
038800070227      * apro i file relativi alle visite
038900110103     c**                 if        i45ast = 'V' or i45ast = 'T'
039000110103     c                   if        i45ast = 'T'
039100070227     c                   open      tfaco00f
039200070227     c                   open      tfind00f
039300070227     c                   open      tfclp00f
039400110103     c**                 if        i45ast = 'V'
039500110103     c**                 open      tnvis05l
039600110103     c**                 endif
039700091022     c                   if        i45ast = 'T'
039800091022     c                   open      tivis05l
039900091022     c                   endif
040000070227     c                   endif
040100030508
040200020108      *------------------------------------------------------------------*
040300020108      *               DEFINIZIONI DELLE CHIAVI
040400020108      *------------------------------------------------------------------*
040500020108
040600020108      * --- TABEL00F ---
040700020108     c     tabkey        klist
040800020108     c                   kfld                    codut
040900020108     c                   kfld                    cod
041000020108     c                   kfld                    key
041100050216
041200050216      * --- TFNTC ---
041300050216     c     keyntc        klist
041400050216     c                   kfld                    notapl
041500050216     c                   kfld                    notke1
041600050216     c                   kfld                    notke2
041700050216     c                   kfld                    nottnt
041800060818
041900060818      * --- TFACO ---
042000060818     c     keyaco        klist
042100060818     c                   kfld                    codut
042200060818     c                   kfld                    dutkci
042300060818     c                   kfld                    i45nrv
042400020108
042500020108     c                   endsr
