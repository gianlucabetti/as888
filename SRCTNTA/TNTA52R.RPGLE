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
002100020625      *--------
002200020625      * Ds di riferimento al file esterno AzUte00F
002300020625      *--------
002400020625     d AzuteDs       e ds                  ExtName(AzUte00F)
002500020625      *--------
002600020625      * Ds per dati organigramma
002700020625      *--------
002800020625     d DDatiUte      e ds
002900020625      *--------
003000020625      * Parametri per richiamo al pgm di controllo profilo utenti
003100020625      *--------
003200020625     d Tibs34Ds      e ds                  Inz
003300020625
003400020111
003500020107     D KPJBA         E DS
003600020107     D  utente                 1      5
003700020111
003800020107     D FNLV45DS      E DS
003900021022     D fnlv41ds      E DS
004000020111
004100050216      *------------------------------------------------------------------*
004200050216      *  V A R I A B I L I
004300050216      *------------------------------------------------------------------*
004400050216     d cod             s                   like(tblcod)
004500050216     D comfil          s              3  0
004600020107     D DATA_eur        S               D   DATFMT(*eur)
004700050216     D datastampa      s             10
004800050216     D key             s                   like(tblkey)
004900050216     D notapl          s                   like(ntcapl)
005000050216     D notke1          s                   like(ntcnk1)
005100050216     D notke2          s                   like(ntcnk2)
005200050216     D nottnt          s                   like(ntctnt)
005300050216     D w003a           S              3
005400020111
005500020108      *------------------------------------------------------------------*
005600020108      *               RIEPILOGO INDICATORI
005700020108      *------------------------------------------------------------------*
005800050216      * 70    - CONDIZIONA LA STAMPA DEI RIFERIMENTI
005900050216      * 73    - CONDIZIONA LA STAMPA DEL RESPONSABILE TRASPORTI
006000020108      * 77    - CONDIZIONA LA STAMPA DEL E.P.C.
006100020109      * 80    - OVERFLOW
006200020108      *------------------------------------------------------------------*
006300010531      *    T E S T A T A    S T A M P A    T A R I F F A
006400020108      *------------------------------------------------------------------*
006500021022      *
006600050216      * Per errore di reperimento testo vado a fine pgm
006700021022     C                   if        $NoStp   = *on
006800021022     C                   goto      fine
006900021022     C                   endif
007000020108
007100020108      * Recupero il cliente
007200020108     c                   exsr      Sr_Reccli
007300020108
007400020108      * Decodifico i dati del cliente
007500020108     c                   exsr      Sr_Decana
007600130521
007700050216      * Recupero note per responsabile trasporti
007800050216     c                   exsr      Sr_Recnot
007900020108
008000130521      * Imposto località e data di stampa
008100130521     c                   exsr      Sr_DataSt
008200020108
008300020108      * stampo la prima parte della tariffa
008400050216     c                   write     ta52pPP
008500050216     c                   write     ta52p01
008600020109
008700020109     c     fine          tag
008800050216
008900050216     C                   eval      I69TLA  = 'C'
009000050216     C                   call      'TIBS69R'
009100050216     C                   parm                    TIBS69DS
009200050216
009300020109     c                   seton                                        lr
009400020109
009500020108      *------------------------------------------------------------------*
009600020108      *   Sr_Reccli       RECUPERO CLIENTE
009700020108      *------------------------------------------------------------------*
009800020108
009900020108     c     Sr_Reccli     begsr
010000020108
010100050216     c                   clear                   TIBS69DS
010200050216     c                   move      parsks        I69kac
010300050216     c                   move      parsks        I69kin
010400050216     c                   move      parsks        I69kcp
010500050216     c                   call      'TIBS69R'
010600050216     c                   parm                    TIBS69DS
010700050216     c                   parm                    DS_CNACO
010800050216     c                   parm                    DS_CNIND
010900050216     c                   parm                    DS_CNCLP
011000050216     c                   parm                    DS_FNCLS
011100050216if  2c                   if        O69err <> *on
011200010622      * se non c'è errore valorizzo i campi in stampa
011300010622     c                   eval      picrag = acorag
011400010622     c                   eval      picind = indvia
011500130517     c*//                eval      piccap = indcae
011600130517     c*//                eval      picloc = indcit
011700130517     c*//                eval      picpro = indprv
011800130517     c                   eval      PICcalopr = %trimr(INDcae) + '  '
011900130517     c                                       + %trimr(INDcit)
012000130517     c                   eval      %subst(PICcalopr : %size(PICcalopr) - 2)
012100130517     c                                       = ' ' + INDprv
012200050216     c                   Else
012300050216     C                   goto      fine
012400050216     c                   endif
012500020108
012600020108     c                   endsr
012700020108
012800020108      *------------------------------------------------------------------*
012900020108      *   Sr_Decana       DECODIFICA DATI CLIENTI PER LA STAMPA
013000020108      *------------------------------------------------------------------*
013100020108
013200020108     c     Sr_Decana     begsr
013300050216
013400130530     c*//                Eval      *In70 = *Off
013500050216
013600130530      *//* valorizzo i numeri riferimento
013700130530     c*//                eval      picnr1 = i45nr1
013800130530     c*//                eval      picnr2 = i45nr2
013900130530     c*//                If        picnr1 <> *Blanks or picnr2 <> *Blanks
014000130530     c*//                Eval      *In70 = *On
014100130530     c*//                EndIf
014200020108
014300020108      * decodfico la nazione se diversa da italia
014400020108
014500020108     c                   if        indsta <> *blanks
014600020108     c                   eval      cod ='15'
014700020108     c                   movel     indsta        w003a
014800020108     c                   movel(p)  w003a         key
014900020108     c     tabkey        chain     tabel00f
015000020108     c                   if        %found(tabel00f)
015100020108     c                   movel     tbluni        desnar
015200020108     c                   else
015300020108     c                   clear                   desnar
015400020108     c                   endif
015500020108
015600020108     c                   endif
015700020108
015800020108     c                   endsr
015900020108
016000050216      *------------------------------------------------------------------*
016100050216      *   Sr_Recnot       RECUPERA NOTE CLIENTI
016200050216      *------------------------------------------------------------------*
016300050216
016400050216     c     Sr_Recnot     begsr
016500050216
016600050216     c                   eval      *in73 = *Off
016700050216     c                   eval      *in77 = *Off
016800050216
016900050216      * chiave note tariffe/offerta cliente
017000050216
017100050216     c                   eval      notapl = 'C'
017200050216
017300050216      * Responsabile trasporti
017400050216     c                   movel     '05'          nottnt
017500050216     c                   movel     i45kcc        notke1
017600050216     c                   move      parsks        notke1
017700091113     c     keyntc        setll     tfntc01l
017800050216     c                   do        *hival
017900050216     c     keyntc        reade     tfntc01l
018000050216
018100050216     c                   if        %eof(tfntc01l)
018200050216     c                   leave
018300050216     c                   endif
018400050216      * se annullato oppure è una nota interna leggo la successiva
018500050216     c                   if        ntcflt = 'A' or ntcsns <> 'S'
018600050216     c                   iter
018700050216     c                   endif
018800050216      * se alla c.a. è uguale a blank carico la nota
018900050216     c                   if        resptra = *blanks
019000050216     c                   movel     ntcrnt        resptra
019100050216     c                   seton                                        73
019200050216      *
019300050216     c                   else
019400050216      * se è già pieno scrivo nel nominativo del e.p.c. ed esco
019500130517     c*//                movel     ntcrnt        resptra2
019600050216     c                   seton                                        77
019700050216     c                   leave
019800050216     c                   endif
019900050216
020000050216     c                   enddo
020100050216
020200050216     c                   endsr
020300050216
020400020108      *------------------------------------------------------------------*
020500130521      *   Sr_DataSt       IMPOSTA LOCALITÀ E DATA DI STAMPA
020600020108      *------------------------------------------------------------------*
020700020108
020800130521     c     Sr_DataSt     begsr
020900020108
021000050216      * recupero il p.o. appartenenza
021100020108     c                   movel     parsks        comfil
021200020108
021300020108     c     comfil        chain     azorg01l
021400130521
021500130521     c                   if        %found(azorg01l)
021600130530     c                   movel     orgdes        PD1fil
021700130530     c                   if        I45lin = '2'
021800130530     c                   eval      PD1fil = %trimr(ORGdes) + ' branch'
021900130530     c                   endif
022000130521     c                   endif
022100020108
022200020108      * costruisco la data stampa
022300020108     c                   movel     i45gds        data_eur
022400020108     c     *eur          movel     data_eur      datastampa
022500130521      *//* imposto nelle varie lingue
022600130521     c*//                select
022700130521     c*//                when      i45lin = 'I'
022800130521     c*//                eval      picdal = %trim(orgLOC) + ', li ' +
022900130521     c*//                                   datastampa
023000130521     c*//                when      i45lin = '2'
023100130521     c*//                eval      picdal = %trim(orgLOC) + ', ' +
023200130521     c*//                                   datastampa
023300130521     c*//                when      i45lin = '3'
023400130521     c*//                eval      picdal = %trim(orgLOC) + ', le ' +
023500130521     c*//                                   datastampa
023600130521     c*//                when      i45lin = '4'
023700130521     c*//                eval      picdal = %trim(orgLOC) + ', ' +
023800130521     c*//                                   datastampa
023900130521     c*//                endsl
024000130521     c                   eval      PICdal = %trim(orgLOC) + ', ' +
024100130521     c                                      datastampa
024200020108
024300020108     c                   endsr
024400020108
024500020108      *------------------------------------------------------------------*
024600020108      *  Sr_Cartab        CARICAMENTO INIZIALE TABELLE
024700020108      *------------------------------------------------------------------*
024800020108
024900020108     c     Sr_Cartab     begsr
025000021022      *
025100021023      * Richiamo FnLv41 per verificare/creare testo e reperire ds1h
025200021022     C                   clear                   Ds1H
025300021022     C                   move      *off          $NoStp            1
025400021022     C                   clear                   Fnlv41ds
025500021022     C                   movel     '3'           I41Mod
025600021022     C                   movel     I45TTX        I41Let
025700021022     C                   CALL      'FNLV41R'
025800021022     C                   PARM                    FNLV41DS
025900021022      *
026000021022      * Se non ho errori stampo
026100021023     C     o41err        ifne      'E'
026200021022     C                   movel     o41dta        DS1H
026300021022     C                   ELSE
026400021022     C                   move      *on           $NoStp
026500021022     C                   ENDIF
026600031120
026700020107
026800020108     c                   endsr
026900020107
027000020110      *------------------------------------------------------------------*
027100020110      *                   ROUTINE INIZIALE
027200020110      *------------------------------------------------------------------*
027300020108
027400020108     c     *inzsr        begsr
027500020108
027600020108     c     *entry        plist
027700020108     c                   parm                    kpjba
027800020108     c                   parm                    fnlv45ds
027900020108
028000020625     c                   z-add     1             codut             1 0
028100020625
028200020625      * reperisco i dati utente
028300020625     c     *dtaara       define    §azute        AzuteDs
028400020625     c     *dtaara       define    §datiute      DDatiUte
028500020625
028600020625     c                   in(E)     *dtaara
028700020625     c                   IF        %Error or RSUT = *blanks
028800020625     c                   call      'TIBS34R'
028900020625     c                   parm                    Tibs34Ds
029000020625     c                   in        *dtaara
029100020625     c                   ENDIF
029200050216
029300020107      * carico le tabelle
029400020107     c                   exsr      Sr_Cartab
029500021022      *
029600021022      * proseguo se ho i dati per uso testi
029700021022     c                   if        $NoStp = *off
029800020108
029900020108      * valorizzo il campo del codice cliente tariffa
030000020108     c                   eval      parsks = i45ksc
030100020108
030200021022     c                   endif
030300030508
030400030508
030500020108      *------------------------------------------------------------------*
030600020108      *               DEFINIZIONI DELLE CHIAVI
030700020108      *------------------------------------------------------------------*
030800020108
030900020108      * --- TABEL00F ---
031000020108     c     tabkey        klist
031100020108     c                   kfld                    codut
031200020108     c                   kfld                    cod
031300020108     c                   kfld                    key
031400050216
031500050216      * --- TFNTC ---
031600050216     c     keyntc        klist
031700050216     c                   kfld                    notapl
031800050216     c                   kfld                    notke1
031900050216     c                   kfld                    notke2
032000050216     c                   kfld                    nottnt
032100020108
032200020108     c                   endsr
