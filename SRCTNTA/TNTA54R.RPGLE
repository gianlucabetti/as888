000100041230     H DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200020108      *------------------------------------------------------------------*
000300050216      *        S T A M P A       L E T T E R A
000400060818      *        TESTO 05
000500020108      *------------------------------------------------------------------*
000600050216     FTFNTC01L  IF   E           K DISK
000700130905     fAZCMM01L  if   e           k disk
000800020107     FAZORG01L  IF   E           K DISK
000900020107     FTABEL00F  IF   E           K DISK
001000070227     FTFACO00F  IF   E           K DISK    usropn
001100070227     FTFIND00F  IF   E           K DISK    usropn
001200070227     FTFCLP00F  IF   E           K DISK    usropn
001300091022     fTivis05l  if   e           k disk    usropn
001400060818     FTNTA54P   O    E             PRINTER oflind(*in80)
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
018200130905      *
018300020108     c     Sr_Decana     begsr
018400130905      *
018500050216     c                   Eval      *In70 = *Off
018600130905      *
018700050216      * valorizzo i numeri riferimento
018800050216     c                   eval      picnr1 = i45nr1
018900050216     c                   eval      picnr2 = i45nr2
019000050216     c                   If        picnr1 <> *Blanks or picnr2 <> *Blanks
019100050216     c                   Eval      *In70 = *On
019200050216     c                   EndIf
019300130905      *
019400020108      * decodfico la nazione se diversa da italia
019500130905      *
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
020600130905      *
020700020108     c                   endif
020800130905      *
020900020108      * decodifico il commerciale
021000130905     c                   clear                   wCMMcod
021100060818      * se è un cliente lo recupero dall'anagrafica
021200060818     c                   if        i45ksc > *zeros
021300060818     c                   if        clpage = 0
021400060818     c                   leavesr
021500060818     c                   endif
021600130905     c                   z-add     clpage        wCMMcod
021700060818      * se è una visita lo recupero dalla visita
021800060818     c                   else
021900060818     c                   if        viscmm = 0
022000060818     c                   leavesr
022100060818     c                   endif
022200130905     c                   z-add     viscmm        wCMMcod
022300060818     c                   endif
022400130905      *
022500130905     c     wCMMcod       chain     AZCMM000
022600130905     c                   if        %found(AZCMM01L)
022700130905     c                   movel     CMMdes        descom
022800020108     c                   else
022900020108     c                   clear                   descom
023000020108     c                   endif
023100130905      *
023200020108     c                   endsr
023300020108
023400050216      *------------------------------------------------------------------*
023500050216      *   Sr_Recnot       RECUPERA NOTE CLIENTI
023600050216      *------------------------------------------------------------------*
023700050216
023800050216     c     Sr_Recnot     begsr
023900050216
024000050216     c                   eval      *in73 = *Off
024100050216     c                   eval      *in77 = *Off
024200050216
024300050216      * chiave note tariffe/offerta cliente
024400060818     c                   movel     i45kcc        notke1
024500060818      * se cliente
024600060818     c                   if        i45ksc <> *zeros
024700050216     c                   eval      notapl = 'C'
024800060818     c                   move      i45ksc        notke1
024900091113      * se visita/trattativa
025000060818     c                   else
025100091113     c                   eval      notapl = i45ast
025200060818     c                   move      i45nrv        notke1
025300091113      * se trattativa su cliente cerco con il cliente
025400091113     c                   if        $trattativa and visksc > 0
025500091113     c                   eval      notapl = 'C'
025600091113     c                   move      visksc        notke1
025700091113     c                   endif
025800060818     c                   endif
025900050216
026000050216      * Responsabile trasporti
026100050216     c                   movel     '05'          nottnt
026200050216      * cerco prima con in chiave il codice tariffa
026300050216     c     keyntc        setll     tfntc01l
026400050216     c                   do        *hival
026500050216     c     keyntc        reade     tfntc01l
026600050216
026700050216     c                   if        %eof(tfntc01l)
026800050216     c                   leave
026900050216     c                   endif
027000050216      * se annullato oppure è una nota interna leggo la successiva
027100050216     c                   if        ntcflt = 'A' or ntcsns <> 'S'
027200050216     c                   iter
027300050216     c                   endif
027400050216      * se alla c.a. è uguale a blank carico la nota
027500050216     c                   if        resptra = *blanks
027600050216     c                   movel     ntcrnt        resptra
027700050216     c                   seton                                        73
027800050216      *
027900050216     c                   else
028000050216      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
028100050216     c                   movel     ntcrnt        resptra2
028200050216     c                   seton                                        77
028300050216     c                   leave
028400050216     c                   endif
028500050216
028600050216     c                   enddo
028700050216
028800050216     c                   endsr
028900050216
029000020108      *------------------------------------------------------------------*
029100020108      *   Sr_Decpo        DECODIFICA I DATI DEL PUNTO OPERATIVO
029200020108      *------------------------------------------------------------------*
029300020108
029400020108     c     Sr_Decpo      begsr
029500020108
029600050216      * recupero il p.o. appartenenza
029700020108
029800060818     c                   if        i45ksc <> *zeros
029900060818     c                   movel     i45ksc        comfil
030000060818     c                   else
030100060818     c                   movel     viscmm        comfil
030200060818     c                   endif
030300020108
030400020108      * decodifica
030500020108
030600020108     c     comfil        chain     azorg01l
030700020108
030800020108     c                   if        %found(azorg01l)
030900020108     c                   movel     orgdes        td1fil
031000050216     c                   movel     orgdes        pd1fil
031100020108     c                   movel     orgind        td1via
031200020108     c                   movel     orgcpf        td1cap
031300020108     c                   movel     orgloc        td1loc
031400020108     c                   movel     orgpro        td1prv
031500020108     c                   movel     orgtel        td2tel
031600020108     c                   movel     orgfax        td2tfx
031700020108     c                   endif
031800020108
031900020108      * costruisco la data stampa
032000020108     c                   movel     i45gds        data_eur
032100020108     c     *eur          movel     data_eur      datastampa
032200060613      * imposto nelle varie lingue
032300060613     c                   select
032400060613     c                   when      i45lin = 'I'
032500020108     c                   eval      picdal = %trim(orgdes) + ',li ' + datastampa
032600060613     c                   when      i45lin = '2'
032700060613     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
032800060818     c                   when      i45lin = '3'
032900060818     c                   eval      picdal = %trim(orgdes) + ',le ' + datastampa
033000060818     c                   when      i45lin = '4'
033100060818     c                   eval      picdal = %trim(orgdes) + ', ' + datastampa
033200060613     c                   endsl
033300020108
033400020108     c                   endsr
033500020108
033600020108      *------------------------------------------------------------------*
033700020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
033800020108      *------------------------------------------------------------------*
033900020108
034000020108     c     Sr_Cartab     begsr
034100021022      *
034200021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
034300130905     ***C                   clear                   Ds1H
034400021022     C                   move      *off          $NoStp            1
034500021022     C                   clear                   Fnlv41ds
034600021022     C                   movel     '3'           I41Mod
034700021022     C                   movel     I45TTX        I41Let
034800021022     C                   CALL      'FNLV41R'
034900021022     C                   PARM                    FNLV41DS
035000021022      *
035100021022      * Se non ho errori stampo
035200021023     C     o41err        ifne      'E'
035300130905     ***C                   movel     o41dta        DS1H
035400021022     C                   ELSE
035500021022     C                   move      *on           $NoStp
035600021022     C                   ENDIF
035700031120
035800020108     c                   endsr
035900020107
036000020110      *------------------------------------------------------------------*
036100020110      *                   ROUTINE INIZIALE
036200020110      *------------------------------------------------------------------*
036300020108
036400020108     c     *inzsr        begsr
036500020108
036600020108     c     *entry        plist
036700020108     c                   parm                    kpjba
036800020108     c                   parm                    fnlv45ds
036900020108
037000020625     c                   z-add     1             codut             1 0
037100020625
037200020625      * reperisco i dati utente
037300020625     c     *dtaara       define    §azute        AzuteDs
037400020625     c     *dtaara       define    §datiute      DDatiUte
037500020625
037600020625     c                   in(E)     *dtaara
037700020625     c                   IF        %Error or RSUT = *blanks
037800020625     c                   call      'TIBS34R'
037900020625     c                   parm                    Tibs34Ds
038000020625     c                   in        *dtaara
038100020625     c                   ENDIF
038200050216
038300020107      * carico le tabelle
038400020107     c                   exsr      Sr_Cartab
038500070227
038600070227      * apro i file relativi alle visite
038700110103     c**                 if        i45ast = 'V' or i45ast = 'T'
038800110103     c                   if        i45ast = 'T'
038900070227     c                   open      tfaco00f
039000070227     c                   open      tfind00f
039100070227     c                   open      tfclp00f
039200110103     c**                 if        i45ast = 'V'
039300110103     c**                 open      tnvis05l
039400110103     c**                 endif
039500091022     c                   if        i45ast = 'T'
039600091022     c                   open      tivis05l
039700091022     c                   endif
039800070227     c                   endif
039900030508
040000020108      *------------------------------------------------------------------*
040100020108      *               DEFINIZIONI DELLE CHIAVI
040200020108      *------------------------------------------------------------------*
040300020108
040400020108      * --- TABEL00F ---
040500020108     c     tabkey        klist
040600020108     c                   kfld                    codut
040700020108     c                   kfld                    cod
040800020108     c                   kfld                    key
040900050216
041000050216      * --- TFNTC ---
041100050216     c     keyntc        klist
041200050216     c                   kfld                    notapl
041300050216     c                   kfld                    notke1
041400050216     c                   kfld                    notke2
041500050216     c                   kfld                    nottnt
041600060818
041700060818      * --- TFACO ---
041800060818     c     keyaco        klist
041900060818     c                   kfld                    codut
042000060818     c                   kfld                    dutkci
042100060818     c                   kfld                    i45nrv
042200020108
042300020108     c                   endsr
