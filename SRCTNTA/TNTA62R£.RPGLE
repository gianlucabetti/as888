000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300050216      *        S T A M P A       L E T T E R A
000400060906      *        TESTO 07
000500020108      *------------------------------------------------------------------*
000600050216     FTFNTC01L  IF   E           K DISK
000700020107     FAZORG01L  IF   E           K DISK
000800020107     FTABEL00F  IF   E           K DISK
000900070227     FTFACO00F  IF   E           K DISK    usropn
001000070227     FTFIND00F  IF   E           K DISK    usropn
001100070227     FTFCLP00F  IF   E           K DISK    usropn
001200091022     fTivis05l  if   e           k disk    usropn
001300060905     FTNTA62P   O    E             PRINTER oflind(*in80)
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
006000091116     d $trattativa     s              1n
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
009700060905     c                   write     ta62pPP
009800060905     c                   write     ta62p01
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
011300020108
011400060818      * se è stato passato il numero della visita controllo se devo recuperare
011500060818      * i dati dall'anagrafica provvisoria
011600060818     c                   if        i45nrv <> *zeros
011700091022     c                   select
011800110103     c**                 when      i45ast = 'V'
011900060818      * con la vistita vado su tnvis
012000110103     c**   i45nrv        chain     tnvis05l
012100091022      * con la trattativa vado su tivis
012200091022     c                   when      i45ast = 'T'
012300091022     c     i45nrv        chain     tivis05l
012400091022     c                   endsl
012500060818      * se non esite la visita esco dal programma, ma non dovrebbe capitare
012600091022     c                   if        not %found
012700060818     c                   goto      fine
012800060818     c                   endif
012900060818      * controllo se esiste l'anagrafica provvisoria
013000060818     c     keyaco        chain     tfaco00f
013100060818     c                   if        not %found(tfaco00f)
013200060818      * se non esite l'anagrafica provvisoria e non è una visita di un cliente
013300060818      * codificato esco dal programma, ma non dovrebbe capitare
013400060818     c                   if        visksc = *zeros
013500060818     c                   goto      fine
013600060818     c                   endif
013700060818      * se non esiste l'anagrafica provvisoria ed è una visita di un cliente
013800060818      * codificato recupero i dato dall'anagrafica clienti
013900060818     c                   eval      i45ksc = visksc
014000060818     c                   goto      waco
014100060818     c                   endif
014200060818      * imposto i dati da stampare
014300060818     c     keyaco        chain     tfind00f
014400060818     c                   eval      picrag = acorag
014500060818     c                   eval      picind = indvia
014600060818     c                   eval      piccap = indcae
014700060818     c                   eval      picloc = indcit
014800060818     c                   eval      picpro = indprv
014900060818     c                   endif
015000060818
015100060818     c     waco          tag
015200060818
015300050216     c                   clear                   TIBS69DS
015400060818     c                   move      i45ksc        I69kac
015500060818     c                   move      i45ksc        I69kin
015600060818     c                   move      i45ksc        I69kcp
015700050216     c                   call      'TIBS69R'
015800050216     c                   parm                    TIBS69DS
015900050216     c                   parm                    DS_CNACO
016000050216     c                   parm                    DS_CNIND
016100050216     c                   parm                    DS_CNCLP
016200050216     c                   parm                    DS_FNCLS
016300050216if  2c                   if        O69err <> *on
016400010622      * se non c'è errore valorizzo i campi in stampa
016500010622     c                   eval      picrag = acorag
016600010622     c                   eval      picind = indvia
016700010622     c                   eval      piccap = indcae
016800010622     c                   eval      picloc = indcit
016900010622     c                   eval      picpro = indprv
017000050216     c                   Else
017100050216     C                   goto      fine
017200050216     c                   endif
017300020108
017400020108     c                   endsr
017500020108
017600020108      *------------------------------------------------------------------*
017700020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
017800020108      *------------------------------------------------------------------*
017900020108
018000020108     c     Sr_Decana     begsr
018100050216
018200050216     c                   Eval      *In70 = *Off
018300050216
018400050216      * valorizzo i numeri riferimento
018500050216     c                   eval      picnr1 = i45nr1
018600050216     c                   eval      picnr2 = i45nr2
018700050216     c                   If        picnr1 <> *Blanks or picnr2 <> *Blanks
018800050216     c                   Eval      *In70 = *On
018900050216     c                   EndIf
019000020108
019100020108      * decodfico la nazione se diversa da italia
019200020108
019300020108     c                   if        indsta <> *blanks
019400020108     c                   eval      cod ='15'
019500020108     c                   movel     indsta        w003a
019600020108     c                   movel(p)  w003a         key
019700020108     c     tabkey        chain     tabel00f
019800020108     c                   if        %found(tabel00f)
019900020108     c                   movel     tbluni        desnar
020000020108     c                   else
020100020108     c                   clear                   desnar
020200020108     c                   endif
020300020108
020400020108     c                   endif
020500020108
020600020108      * decodifico il commerciale
020700020108     c                   clear                   key
020800060818      * se è un cliente lo recupero dall'anagrafica
020900060818     c                   if        i45ksc > *zeros
021000060818     c                   if        clpage = 0
021100060818     c                   leavesr
021200060818     c                   endif
021300020108     c                   movel(p)  clpage        key
021400060818      * se è una visita lo recupero dalla visita
021500060818     c                   else
021600060818     c                   if        viscmm = 0
021700060818     c                   leavesr
021800060818     c                   endif
021900060818     c                   movel(p)  viscmm        key
022000060818     c                   endif
022100020108
022200020108     c                   eval      cod = '01'
022300020108     c     tabkey        chain     tabel00f
022400020108     c                   if        %found(tabel00f)
022500020108     c                   movel     tbluni        descom
022600020108     c                   else
022700020108     c                   clear                   descom
022800020108     c                   endif
022900020108
023000020108     c                   endsr
023100020108
023200050216      *------------------------------------------------------------------*
023300050216      *   Sr_Recnot       RECUPERA NOTE CLIENTI
023400050216      *------------------------------------------------------------------*
023500050216
023600050216     c     Sr_Recnot     begsr
023700050216
023800050216     c                   eval      *in73 = *Off
023900050216     c                   eval      *in77 = *Off
024000050216
024100050216      * chiave note tariffe/offerta cliente
024200060818     c                   movel     i45kcc        notke1
024300060818      * se cliente
024400060818     c                   if        i45ksc <> *zeros
024500050216     c                   eval      notapl = 'C'
024600060818     c                   move      i45ksc        notke1
024700060818     c                   else
024800060818     c                   eval      notapl = 'V'
024900091116     c                   if        $trattativa
025000091116     c                   eval      notapl = 'T'
025100091116     c                   endif
025200060818     c                   move      i45nrv        notke1
025300060818     c                   endif
025400050216
025500050216      * Responsabile trasporti
025600050216
025700050216     c                   movel     '05'          nottnt
025800050216      * cerco prima con in chiave il codice tariffa
025900050216     c     keyntc        setll     tfntc01l
026000050216     c                   do        *hival
026100050216     c     keyntc        reade     tfntc01l
026200050216
026300050216     c                   if        %eof(tfntc01l)
026400050216     c                   leave
026500050216     c                   endif
026600050216      * se annullato oppure è una nota interna leggo la successiva
026700050216     c                   if        ntcflt = 'A' or ntcsns <> 'S'
026800050216     c                   iter
026900050216     c                   endif
027000050216      * se alla c.a. è uguale a blank carico la nota
027100050216     c                   if        resptra = *blanks
027200050216     c                   movel     ntcrnt        resptra
027300050216     c                   seton                                        73
027400050216      *
027500050216     c                   else
027600050216      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
027700050216     c                   movel     ntcrnt        resptra2
027800050216     c                   seton                                        77
027900050216     c                   leave
028000050216     c                   endif
028100050216
028200050216     c                   enddo
028300050216
028400050216     c                   endsr
028500050216
028600020108      *------------------------------------------------------------------*
028700020108      *   Sr_Decpo        DECODIFICA I DATI DEL PUNTO OPERATIVO
028800020108      *------------------------------------------------------------------*
028900020108
029000020108     c     Sr_Decpo      begsr
029100020108
029200050216      * recupero il p.o. appartenenza
029300020108
029400060818     c                   if        i45ksc <> *zeros
029500060818     c                   movel     i45ksc        comfil
029600060818     c                   else
029700060818     c                   movel     viscmm        comfil
029800060818     c                   endif
029900020108
030000020108      * decodifica
030100020108
030200020108     c     comfil        chain     azorg01l
030300020108
030400020108     c                   if        %found(azorg01l)
030500020108     c                   movel     orgdes        td1fil
030600050216     c                   movel     orgdes        pd1fil
030700020108     c                   movel     orgind        td1via
030800020108     c                   movel     orgcpf        td1cap
030900020108     c                   movel     orgloc        td1loc
031000020108     c                   movel     orgpro        td1prv
031100020108     c                   movel     orgtel        td2tel
031200020108     c                   movel     orgfax        td2tfx
031300020108     c                   movel     orgde3        og143
031400020108     c                   endif
031500020108
031600020108      * costruisco la data stampa
031700020108     c                   movel     i45gds        data_eur
031800020108     c     *eur          movel     data_eur      datastampa
031900060613      * imposto nelle varie lingue
032000060613     c                   select
032100060613     c                   when      i45lin = 'I'
032200020108     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
032300060613     c                   when      i45lin = '2'
032400060613     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
032500060818     c                   when      i45lin = '3'
032600060818     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
032700060818     c                   when      i45lin = '4'
032800060818     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
032900060613     c                   endsl
033000020108
033100020108     c                   endsr
033200020108
033300020108      *------------------------------------------------------------------*
033400020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
033500020108      *------------------------------------------------------------------*
033600020108
033700020108     c     Sr_Cartab     begsr
033800021022      *
033900021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
034000021022     C                   clear                   Ds1H
034100021022     C                   move      *off          $NoStp            1
034200021022     C                   clear                   Fnlv41ds
034300021022     C                   movel     '3'           I41Mod
034400021022     C                   movel     I45TTX        I41Let
034500021022     C                   CALL      'FNLV41R'
034600021022     C                   PARM                    FNLV41DS
034700021022      *
034800021022      * Se non ho errori stampo
034900021023     C     o41err        ifne      'E'
035000021022     C                   movel     o41dta        DS1H
035100021022     C                   ELSE
035200021022     C                   move      *on           $NoStp
035300021022     C                   ENDIF
035400031120
035500020108     c                   endsr
035600020107
035700020110      *------------------------------------------------------------------*
035800020110      *                   ROUTINE INIZIALE
035900020110      *------------------------------------------------------------------*
036000020108
036100020108     c     *inzsr        begsr
036200020108
036300020108     c     *entry        plist
036400020108     c                   parm                    kpjba
036500020108     c                   parm                    fnlv45ds
036600020108
036700020625     c                   z-add     1             codut             1 0
036800020625
036900020625      * reperisco i dati utente
037000020625     c     *dtaara       define    §azute        AzuteDs
037100020625     c     *dtaara       define    §datiute      DDatiUte
037200020625
037300020625     c                   in(E)     *dtaara
037400020625     c                   IF        %Error or RSUT = *blanks
037500020625     c                   call      'TIBS34R'
037600020625     c                   parm                    Tibs34Ds
037700020625     c                   in        *dtaara
037800020625     c                   ENDIF
037900050216
038000020107      * carico le tabelle
038100020107     c                   exsr      Sr_Cartab
038200091116      * se trattativa imposto flag
038300091116     c                   eval      $trattativa = (i45ast = 'T')
038400070227
038500070227      * apro i file relativi alle visite
038600110103     c**                 if        i45ast = 'V' or i45ast = 'T'
038700110103     c                   if        i45ast = 'T'
038800070227     c                   open      tfaco00f
038900070227     c                   open      tfind00f
039000070227     c                   open      tfclp00f
039100110103     c**                 if        i45ast = 'V'
039200110103     c**                 open      tnvis05l
039300110103     c**                 endif
039400091022     c                   if        i45ast = 'T'
039500091022     c                   open      tivis05l
039600091022     c                   endif
039700070227     c                   endif
039800030508
039900020108      *------------------------------------------------------------------*
040000020108      *               DEFINIZIONI DELLE CHIAVI
040100020108      *------------------------------------------------------------------*
040200020108
040300020108      * --- TABEL00F ---
040400020108     c     tabkey        klist
040500020108     c                   kfld                    codut
040600020108     c                   kfld                    cod
040700020108     c                   kfld                    key
040800050216
040900050216      * --- TFNTC ---
041000050216     c     keyntc        klist
041100050216     c                   kfld                    notapl
041200050216     c                   kfld                    notke1
041300050216     c                   kfld                    notke2
041400050216     c                   kfld                    nottnt
041500060818
041600060818      * --- TFACO ---
041700060818     c     keyaco        klist
041800060818     c                   kfld                    codut
041900060818     c                   kfld                    dutkci
042000060818     c                   kfld                    i45nrv
042100020108
042200020108     c                   endsr
