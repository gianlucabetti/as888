000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300050216      *        S T A M P A       L E T T E R A
000400060906      *        TESTO 07
000500020108      *------------------------------------------------------------------*
000600050216     FTFNTC01L  IF   E           K DISK
000700130905     fAZCMM01L  if   e           k disk
000800020107     FAZORG01L  IF   E           K DISK
000900020107     FTABEL00F  IF   E           K DISK
001000070227     FTFACO00F  IF   E           K DISK    usropn
001100070227     FTFIND00F  IF   E           K DISK    usropn
001200070227     FTFCLP00F  IF   E           K DISK    usropn
001300091022     fTivis05l  if   e           k disk    usropn
001400060905     FTNTA62P   O    E             PRINTER oflind(*in80)
001500020108
001600020108      *------------------------------------------------------------------*
001700050216      *  D S   E S T E R N E
001800020108      *------------------------------------------------------------------*
001900050216     d TIBS69DS      e ds                  inz
002000050216     d ds_CNACO      e ds                  inz  extname(CNACO00F)
002100050216     d ds_CNIND      e ds                  inz  extname(CNIND00F)
002200050216     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
002300050216     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
002400130905     *** * DS DEL FILE TABEL00F
002500130905     ***D DS1H          E DS
002600020625      *--------
002700020625      * Ds di riferimento al file esterno AzUte00F
002800020625      *--------
002900020625     d AzuteDs       e ds                  ExtName(AzUte00F)
003000020625      *--------
003100020625      * Ds per dati organigramma
003200020625      *--------
003300020625     d DDatiUte      e ds
003400020625      *--------
003500020625      * Parametri per richiamo al pgm di controllo profilo utenti
003600020625      *--------
003700020625     d Tibs34Ds      e ds                  Inz
003800020625
003900020111
004000020107     D KPJBA         E DS
004100020107     D  utente                 1      5
004200020111
004300020107     D FNLV45DS      E DS
004400021022     D fnlv41ds      E DS
004500020111
004600050216      *------------------------------------------------------------------*
004700050216      *  V A R I A B I L I
004800050216      *------------------------------------------------------------------*
004900050216     d cod             s                   like(tblcod)
005000050216     D comfil          s              3  0
005100020107     D DATA_eur        S               D   DATFMT(*eur)
005200050216     D datastampa      s             10
005300050216     D key             s                   like(tblkey)
005400050216     D notapl          s                   like(ntcapl)
005500050216     D notke1          s                   like(ntcnk1)
005600050216     D notke2          s                   like(ntcnk2)
005700050216     D nottnt          s                   like(ntctnt)
005800130905     d wCMMcod         s                   like(CMMcod)   inz
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
020700130905     c                   clear                   wCMMcod
020800060818      * se è un cliente lo recupero dall'anagrafica
020900060818     c                   if        i45ksc > *zeros
021000060818     c                   if        clpage = 0
021100060818     c                   leavesr
021200060818     c                   endif
021300130905     c                   z-add     clpage        wCMMcod
021400060818      * se è una visita lo recupero dalla visita
021500060818     c                   else
021600060818     c                   if        viscmm = 0
021700060818     c                   leavesr
021800060818     c                   endif
021900130905     c                   z-add     viscmm        wCMMcod
022000060818     c                   endif
022100020108
022200130905     c     wCMMcod       chain     AZCMM000
022300130905     c                   if        %found(AZCMM01L)
022400130905     c                   movel     CMMdes        descom
022500020108     c                   else
022600020108     c                   clear                   descom
022700020108     c                   endif
022800020108
022900020108     c                   endsr
023000020108
023100050216      *------------------------------------------------------------------*
023200050216      *   Sr_Recnot       RECUPERA NOTE CLIENTI
023300050216      *------------------------------------------------------------------*
023400050216
023500050216     c     Sr_Recnot     begsr
023600050216
023700050216     c                   eval      *in73 = *Off
023800050216     c                   eval      *in77 = *Off
023900050216
024000050216      * chiave note tariffe/offerta cliente
024100060818     c                   movel     i45kcc        notke1
024200060818      * se cliente
024300060818     c                   if        i45ksc <> *zeros
024400050216     c                   eval      notapl = 'C'
024500060818     c                   move      i45ksc        notke1
024600060818     c                   else
024700060818     c                   eval      notapl = 'V'
024800091116     c                   if        $trattativa
024900091116     c                   eval      notapl = 'T'
025000091116     c                   endif
025100060818     c                   move      i45nrv        notke1
025200060818     c                   endif
025300050216
025400050216      * Responsabile trasporti
025500050216
025600050216     c                   movel     '05'          nottnt
025700050216      * cerco prima con in chiave il codice tariffa
025800050216     c     keyntc        setll     tfntc01l
025900050216     c                   do        *hival
026000050216     c     keyntc        reade     tfntc01l
026100050216
026200050216     c                   if        %eof(tfntc01l)
026300050216     c                   leave
026400050216     c                   endif
026500050216      * se annullato oppure è una nota interna leggo la successiva
026600050216     c                   if        ntcflt = 'A' or ntcsns <> 'S'
026700050216     c                   iter
026800050216     c                   endif
026900050216      * se alla c.a. è uguale a blank carico la nota
027000050216     c                   if        resptra = *blanks
027100050216     c                   movel     ntcrnt        resptra
027200050216     c                   seton                                        73
027300050216      *
027400050216     c                   else
027500050216      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
027600050216     c                   movel     ntcrnt        resptra2
027700050216     c                   seton                                        77
027800050216     c                   leave
027900050216     c                   endif
028000050216
028100050216     c                   enddo
028200050216
028300050216     c                   endsr
028400050216
028500020108      *------------------------------------------------------------------*
028600020108      *   Sr_Decpo        DECODIFICA I DATI DEL PUNTO OPERATIVO
028700020108      *------------------------------------------------------------------*
028800020108
028900020108     c     Sr_Decpo      begsr
029000020108
029100050216      * recupero il p.o. appartenenza
029200020108
029300060818     c                   if        i45ksc <> *zeros
029400060818     c                   movel     i45ksc        comfil
029500060818     c                   else
029600060818     c                   movel     viscmm        comfil
029700060818     c                   endif
029800020108
029900020108      * decodifica
030000020108
030100020108     c     comfil        chain     azorg01l
030200020108
030300020108     c                   if        %found(azorg01l)
030400020108     c                   movel     orgdes        td1fil
030500050216     c                   movel     orgdes        pd1fil
030600020108     c                   movel     orgind        td1via
030700020108     c                   movel     orgcpf        td1cap
030800020108     c                   movel     orgloc        td1loc
030900020108     c                   movel     orgpro        td1prv
031000020108     c                   movel     orgtel        td2tel
031100020108     c                   movel     orgfax        td2tfx
031200020108     c                   endif
031300020108
031400020108      * costruisco la data stampa
031500020108     c                   movel     i45gds        data_eur
031600020108     c     *eur          movel     data_eur      datastampa
031700060613      * imposto nelle varie lingue
031800060613     c                   select
031900060613     c                   when      i45lin = 'I'
032000020108     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
032100060613     c                   when      i45lin = '2'
032200060613     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
032300060818     c                   when      i45lin = '3'
032400060818     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
032500060818     c                   when      i45lin = '4'
032600060818     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
032700060613     c                   endsl
032800020108
032900020108     c                   endsr
033000020108
033100020108      *------------------------------------------------------------------*
033200020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
033300020108      *------------------------------------------------------------------*
033400020108
033500020108     c     Sr_Cartab     begsr
033600021022      *
033700021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
033800130905     ***C                   clear                   Ds1H
033900021022     C                   move      *off          $NoStp            1
034000021022     C                   clear                   Fnlv41ds
034100021022     C                   movel     '3'           I41Mod
034200021022     C                   movel     I45TTX        I41Let
034300021022     C                   CALL      'FNLV41R'
034400021022     C                   PARM                    FNLV41DS
034500021022      *
034600021022      * Se non ho errori stampo
034700021023     C     o41err        ifne      'E'
034800130905     **C                   movel     o41dta        DS1H
034900021022     C                   ELSE
035000021022     C                   move      *on           $NoStp
035100021022     C                   ENDIF
035200031120
035300020108     c                   endsr
035400020107
035500020110      *------------------------------------------------------------------*
035600020110      *                   ROUTINE INIZIALE
035700020110      *------------------------------------------------------------------*
035800020108
035900020108     c     *inzsr        begsr
036000020108
036100020108     c     *entry        plist
036200020108     c                   parm                    kpjba
036300020108     c                   parm                    fnlv45ds
036400020108
036500020625     c                   z-add     1             codut             1 0
036600020625
036700020625      * reperisco i dati utente
036800020625     c     *dtaara       define    §azute        AzuteDs
036900020625     c     *dtaara       define    §datiute      DDatiUte
037000020625
037100020625     c                   in(E)     *dtaara
037200020625     c                   IF        %Error or RSUT = *blanks
037300020625     c                   call      'TIBS34R'
037400020625     c                   parm                    Tibs34Ds
037500020625     c                   in        *dtaara
037600020625     c                   ENDIF
037700050216
037800020107      * carico le tabelle
037900020107     c                   exsr      Sr_Cartab
038000091116      * se trattativa imposto flag
038100091116     c                   eval      $trattativa = (i45ast = 'T')
038200070227
038300070227      * apro i file relativi alle visite
038400110103     c**                 if        i45ast = 'V' or i45ast = 'T'
038500110103     c                   if        i45ast = 'T'
038600070227     c                   open      tfaco00f
038700070227     c                   open      tfind00f
038800070227     c                   open      tfclp00f
038900110103     c**                 if        i45ast = 'V'
039000110103     c**                 open      tnvis05l
039100110103     c**                 endif
039200091022     c                   if        i45ast = 'T'
039300091022     c                   open      tivis05l
039400091022     c                   endif
039500070227     c                   endif
039600030508
039700020108      *------------------------------------------------------------------*
039800020108      *               DEFINIZIONI DELLE CHIAVI
039900020108      *------------------------------------------------------------------*
040000020108
040100020108      * --- TABEL00F ---
040200020108     c     tabkey        klist
040300020108     c                   kfld                    codut
040400020108     c                   kfld                    cod
040500020108     c                   kfld                    key
040600050216
040700050216      * --- TFNTC ---
040800050216     c     keyntc        klist
040900050216     c                   kfld                    notapl
041000050216     c                   kfld                    notke1
041100050216     c                   kfld                    notke2
041200050216     c                   kfld                    nottnt
041300060818
041400060818      * --- TFACO ---
041500060818     c     keyaco        klist
041600060818     c                   kfld                    codut
041700060818     c                   kfld                    dutkci
041800060818     c                   kfld                    i45nrv
041900020108
042000020108     c                   endsr
