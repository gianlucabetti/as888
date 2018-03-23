000100040611     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200040611
000300040611      *---------------------------------------------------------------*
000400040611
000500040611     fFIVAX01L  if   e           k disk
000600040611      *
000700040611     fFIVAX00E  o    e             disk    rename(FIVAX000:FIVAXERR)
000800040611     f                                     prefix(ERR:3)
000900040611
001000040611      *---------------------------------------------------------------*
001100040611
001200040611      *
001300040611      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
001400040611      *
001500040611     d C_Karnak        c                   const('KARN')
001600040611     d C_Titanedi      c                   const('TITA')
001700040611    *d*C_Generico******c                   const('GEN1')
001800040611     d DigitN          c                   const('1234567890')
001900040611      *
002000040611      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002100040611      *
002200040611     d $Tmd            s                   dim(03)  ctdata   perrcd(1)
002300040611     d                                     like(VAXtmd)
002400040611     d $Trk1           s             80    dim(03)  alt($Tmd)
002500040611     d $Msg            s             80    dim(55)  ctdata   perrcd(1)
002600040611      *
002700040611     d $Trk            s                   dim(40)  inz
002800040611     d                                     like(VAXtrk)
002900040611      *
003000040611      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003100040611      *
003200040611     d FIVAXds       e ds                  inz  extname(FIVAX00F)
003300040611     d   VAX_k                 1     35
003400040611     d SAV_k           s                   inz  like(VAX_k)
003500040611      *
003600040611      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
003700040611      *
003800040611      * - Flags booleani
003900040611     d $Err            s              1    inz(*off)
004000040611      *
004100040611      * - Indici di schiera
004200040611     d xx              s              3  0 inz
004300040611      *
004400040611      * - Campi di comodo
004500040611     d w003a           s              3    inz
004600040611     d w004a           s              4    inz
004700040611     d w006a           s              6    inz
004800040611
004900040611     c     *loval        setll     FIVAX000
005000040611      *
005100040611do  1c                   dou       %eof(FIVAX01L)
005200040611      *
005300040611     c                   read      FIVAX000
005400040611      *
005500040611if  2c                   if        %eof(FIVAX01L)
005600040611     c                   leave
005700040611e   2c                   endif
005800040611      *
005900040611if  2c                   if        VAXatb = '*'
006000040611     c                   iter
006100040611e   2c                   endif
006200040611      *
006300040611     c                   exsr      Ctr_Fld_1
006400040611if  2c                   if             $Err   = *off
006500040611     c                             and (VAXtmd = C_Karnak
006600040611     c                             or   VAXtmd = C_Titanedi)
006700040611     c                   exsr      Ctr_Fld_2
006800040611e   2c                   endif
006900040611      *
007000040611e   1c                   enddo
007100040611      *
007200040611     c                   eval      *inLR  = *on
007300040611
007400040611      **-------------------------------------------------------------**
007500040611      ** Controllo campi "comuni"                                    **
007600040611      **-------------------------------------------------------------**
007700040611     c     Ctr_Fld_1     BegSr
007800040611      *
007900040611     c                   reset                   $Err
008000040611     c                   clear                   $Trk
008100040611      *
008200040611      * P.O. gestione
008300040611     c                   if        VAXfgs <= *zeros
008400040611     c                   eval      xx     =  1
008500040611     c                   exsr      sr_Errore
008600040611     c                   endif
008700040611      *
008800040611      * Codice cliente mittente
008900040611     c                   if        VAXccm <= *zeros
009000040611     c                   eval      xx     =  2
009100040611     c                   exsr      sr_Errore
009200040611     c                   endif
009300040611      *
009400040611      * P.O. partenza
009500040611     c                   if        VAXlnp <= *zeros
009600040611     c                   eval      xx     =  3
009700040611     c                   exsr      sr_Errore
009800040611     c                   endif
009900040611      *
010000040611      * Anno spedizione
010100040611     c                   if           VAXaas <  (*year - 1)
010200040611     c                             or VAXaas >  (*year)
010300040611     c                   eval      xx     =  4
010400040611     c                   exsr      sr_Errore
010500040611     c                   endif
010600040611      *
010700040611      * Numero Serie
010800110111     c                   if        VAXnrs <  *zeros
010900040611     c                   eval      xx     =  5
011000040611     c                   exsr      sr_Errore
011100040611     c                   endif
011200040611      *
011300040611      * Numero Spedizione
011400040611     c                   if        VAXnsp <= *zeros
011500040611     c                   eval      xx     =  6
011600040611     c                   exsr      sr_Errore
011700040611     c                   endif
011800040611      *
011900040611      * Modulo DDT del cliente
012000040611     c                   eval      *in10  =  *off
012100040611     c                   eval      xx     =  1
012200040611if  1c                   if        VAXtmd <> *blanks
012300040611     c     VAXtmd        lookup    $Tmd(xx)                               10
012400040611e   1c                   endif
012500040611if  1c                   if        not *in10
012600040611     c                   eval      xx     =  7
012700040611     c                   exsr      sr_Errore
012800040611x   1c                   else
012900040611      *
013000040611      * Tipo record del DDT
013100040611     c                   movea     $Trk1(xx)     $Trk
013200040611     c                   eval      *in10  =  *off
013300040611     c                   eval      xx     =  1
013400040611if  2c                   if        VAXtrk <> *blanks
013500040611     c     VAXtrk        lookup    $Trk(xx)                               10
013600040611e   2c                   endif
013700040611if  2c                   if        not *in10
013800040611     c                   eval      xx     =  9
013900040611     c                   exsr      sr_Errore
014000040611e   2c                   endif
014100040611e   1c                   endif
014200040611      *
014300040611      * Progressivo riga del DDT
014400040611     c                   if        VAX_k  =  SAV_k
014500040611     c                   eval      xx     =  8
014600040611     c                   exsr      sr_Errore
014700040611     c                   endif
014800040611     c                   movel     VAX_k         SAV_k
014900040611      *
015000040611     c                   EndSr
015100040611
015200040611      **-------------------------------------------------------------**
015300040611      ** Controllo campi "specifici"                                 **
015400040611      **-------------------------------------------------------------**
015500040611     c     Ctr_Fld_2     BegSr
015600040611      *
015700040611sel 1c                   select
015800040611      * - Dati di dettaglio: articolo, descrizione, quantità
015900040611w   1c                   when      VAXtrk = 'C1'
016000040611     c                   exsr      KARN_C1
016100040611      * - Dati di dettaglio: testo per articoli non disponibili
016200040611w   1c                   when      VAXtrk = 'C2'
016300040611     c                   exsr      KARN_C2
016400040611      * - Dati di dettaglio: descrizioni generiche (orario apertura)
016500040611w   1c                   when      VAXtrk = 'C3'
016600040611     c                   exsr      KARN_C3
016700040611      * - Dati di testata: destinatario, cliente di fatturazione
016800040611w   1c                   when      VAXtrk = 'T1'
016900040611     c                   exsr      KARN_T1
017000040611      * - Dati di testata: agente, numero ordine, estremi documento
017100040611w   1c                   when      VAXtrk = 'T2'
017200040611     c                   exsr      KARN_T2
017300040611      * - Dati di fondo: mezzo e causale trasporto, colli, peso
017400040611w   1c                   when      VAXtrk = 'V1'
017500040611     c                   exsr      KARN_V1
017600040611      * - Dati di fondo: vettore, annotazioni e note
017700040611w   1c                   when      VAXtrk = 'V2'
017800040611     c                   exsr      KARN_V2
017900040611      * - Dati esterni: riga descrittiva con referente
018000040611w   1c                   when      VAXtrk = 'L1'
018100040611     c                   exsr      KARN_L1
018200040611      * - Dati di omaggio: testo per articoli omaggiati
018300040611w   1c                   when      VAXtrk = 'O1'
018400040611     c                   exsr      KARN_O1
018500040611      * - Dati di omaggio: articolo, descrizione, quantità
018600040611w   1c                   when      VAXtrk = 'O2'
018700040611     c                   exsr      KARN_O2
018800040611e   1c                   endsl
018900040611      *
019000040611     c                   EndSr
019100040611
019200040611      **-------------------------------------------------------------**
019300040611      ** Controllo dati valori di testata 1 (destinatario, cli.fatt) **
019400040611      **-------------------------------------------------------------**
019500040611     c     KARN_T1       BegSr
019600040611      *
019700040611      * Nome      destinatario
019800040611      * Indirizzo destinatario
019900040611      * CAP       destinatario
020000040611      * Località  destinatario
020100040611      * Provincia destinatario
020200040611      * Nome      cliente di fatturazione
020300040611      * Indirizzo cliente di fatturazione
020400040611      * CAP       cliente di fatturazione
020500040611      * Località  cliente di fatturazione
020600040611      * Provincia cliente di fatturazione
020700040611      *
020800040611     c                   EndSr
020900040611
021000040611      **-------------------------------------------------------------**
021100040611      ** Controllo dati valori di testata 2 (riferimenti mittente)   **
021200040611      **-------------------------------------------------------------**
021300040611     c     KARN_T2       BegSr
021400040611      *
021500040611      * Codice agente             (alfanumerico)
021600040611      * Numero ordine             (alfanumerico)
021700040611      * Data   ordine             (già editata)
021800040611      * Riferimenti               (alfanumerico)
021900040611      * Codice cliente            (numerico)
022000040611     c                   eval      *in10  =  *off
022100040611     c                   eval      w006a  =  %subst(VAXdtt:46:6)
022200040611     c     DigitN        check     w006a                                  10
022300040611if  1c                   if        *in10
022400040611     c                   eval      xx     =  24
022500040611     c                   exsr      sr_Errore
022600040611e   1c                   endif
022700040611      * Estremi documento         (alfanumerico)
022800040611      * Data    documento         (già editata)
022900040611      * Numero pagina             (numerico)
023000040611     c                   eval      *in10  =  *off
023100040611     c                   eval      w003a  =  %subst(VAXdtt:74:3)
023200040611     c     DigitN        check     w003a                                  10
023300040611if  1c                   if        *in10
023400040611     c                   eval      xx     =  27
023500040611     c                   exsr      sr_Errore
023600040611e   1c                   endif
023700040611      * Porto
023800040611      * Codice      pagamento     (numerico)
023900040611     c                   eval      *in10  =  *off
024000040611     c                   eval      w004a  =  %subst(VAXdtt:74:4)
024100040611     c     DigitN        check     w004a                                  10
024200040611if  1c                   if        *in10
024300040611     c                   eval      xx     =  29
024400040611     c                   exsr      sr_Errore
024500040611e   1c                   endif
024600040611      * Descrizione pagamento
024700040611      * Descrizione spedizioniere
024800040611      *
024900040611     c                   EndSr
025000040611
025100040611      **-------------------------------------------------------------**
025200040611      ** Controllo dati valori corpo dettaglio 1 (prod., descr. qtà) **
025300040611      **-------------------------------------------------------------**
025400040611     c     KARN_C1       BegSr
025500040611      *
025600040611      * Codice prodotto           (alfanumerico)
025700040611      * Descrizione prodotto      (alfanumerico)
025800040611      * Quantità                  (già editata)
025900040611      *
026000040611     c                   EndSr
026100040611
026200040611      **-------------------------------------------------------------**
026300040611      ** Controllo dati valori corpo dettaglio 2 (prod. non disp.)   **
026400040611      **-------------------------------------------------------------**
026500040611     c     KARN_C2       BegSr
026600040611      *
026700040611      * Testo
026800040611      *
026900040611     c                   EndSr
027000040611
027100040611      **-------------------------------------------------------------**
027200040611      ** Controllo dati valori corpo dettaglio 3 (descr. generiche)  **
027300040611      **-------------------------------------------------------------**
027400040611     c     KARN_C3       BegSr
027500040611      *
027600040611      * Testo
027700040611      *
027800040611     c                   EndSr
027900040611
028000040611      **-------------------------------------------------------------**
028100040611      ** Controllo dati valori di fondo 1 (peso, cartoni, ecc.)      **
028200040611      **-------------------------------------------------------------**
028300040611     c     KARN_V1       BegSr
028400040611      *
028500040611      * Mezzo di trasporto
028600040611      * Causale trasporto
028700040611      * Numero colli               (già editato)
028800040611      * Peso in kg.                (già editato)
028900040611      * Aspetto esteriore dei beni ("Cartoni")
029000040611      *
029100040611     c                   EndSr
029200040611
029300040611      **-------------------------------------------------------------**
029400040611      ** Controllo dati valori di fondo 2 (vettore, ecc.)            **
029500040611      **-------------------------------------------------------------**
029600040611     c     KARN_V2       BegSr
029700040611      *
029800040611      * Descrizione vettore
029900040611      * Indirizzo   vettore
030000040611      * Città       vettore
030100040611      * Data ritiro
030200040611      * Ora  ritiro
030300040611      * Annotazioni
030400040611      * Note vettore 1
030500040611      * Note vettore 2
030600040611      *
030700040611     c                   EndSr
030800040611
030900040611      **-------------------------------------------------------------**
031000040611      ** Controllo dati valori righe finali (referente, ecc.)        **
031100040611      **-------------------------------------------------------------**
031200040611     c     KARN_L1       BegSr
031300040611      *
031400040611      * Testo
031500040611      *
031600040611     c                   EndSr
031700040611
031800040611      **-------------------------------------------------------------**
031900040611      ** Controllo dati valori di omaggi 1 (testata)                 **
032000040611      **-------------------------------------------------------------**
032100040611     c     KARN_O1       BegSr
032200040611      *
032300040611      * Testo
032400040611      *
032500040611     c                   EndSr
032600040611
032700040611      **-------------------------------------------------------------**
032800040611      ** Controllo dati valori di omaggi 2 (dettaglio)               **
032900040611      **-------------------------------------------------------------**
033000040611     c     KARN_O2       BegSr
033100040611      *
033200040611      * Codice prodotto           (alfanumerico)
033300040611      * Descrizione prodotto      (alfanumerico)
033400040611      * Quantità                  (già editata)
033500040611      *
033600040611     c                   EndSr
033700040611
033800040611      **-------------------------------------------------------------**
033900040611      ** Segnalazione singolo errore                                 **
034000040611      **-------------------------------------------------------------**
034100040611     c     sr_Errore     BegSr
034200040611      *
034300040611     c                   eval      $Err = *on
034400040611      *
034500040611     c                   clear                   FIVAXerr
034600040611     c                   eval      ERRatb = VAXatb
034700040611     c                   eval      ERRfgs = VAXfgs
034800040611     c                   eval      ERRccm = VAXccm
034900040611     c                   eval      ERRlnp = VAXlnp
035000040611     c                   eval      ERRaas = VAXaas
035100040611     c                   eval      ERRnrs = VAXnrs
035200040611     c                   eval      ERRnsp = VAXnsp
035300040611     c                   eval      ERRtmd = VAXtmd
035400040611     c                   eval      ERRprr = VAXprr
035500040611     c                   eval      ERRtrk = VAXtrk
035600040611     c                   movel     $Msg(xx)      ERRdtt
035700040611      *
035800040611     c                   WRITE     FIVAXerr
035900040611      *
036000040611     c                   EndSr
036100040611
036200040611** - $TMD / $TRK
036300040611KARNT1T2C1C2C3V1V2L1O1O2
036400040611TITAT1T2C1C2C3V1V2L1O1O2
036500040611GEN1T1T2C1
036600040611** - $Msg
0367000406111 - P.O. GESTIONE                errato o mancante                               1
0368000406112 - CODICE CLIENTE MITTENTE      errato o mancante                               2
0369000406113 - P.O. PARTENZA                errato o mancante                               3
0370000406114 - ANNO SPEDIZIONE              errato o mancante                               4
0371000406115 - NUMERO SERIE                 errato                                          5
0372000406116 - NUMERO SPEDIZIONE            errato o mancante                               6
0373000406117 - TIPO MODULO DDT CLIENTE      errato o mancante                               7
0374000406118 - PROGRESSIVO RIGA DDT CLIENTE duplicato                                       8
0375000406119 - TIPO RECORD DDT CLIENTE      errato o mancante                               9
037600040611T1  -  Nome      destinatario                                                   10
037700040611T1  -  Indirizzo destinatario                                                   11
037800040611T1  -  CAP       destinatario                                                   12
037900040611T1  -  Località  destinatario                                                   13
038000040611T1  -  Provincia destinatario                                                   14
038100040611T1  -  Nome      cliente di fatturazione                                        15
038200040611T1  -  Indirizzo cliente di fatturazione                                        16
038300040611T1  -  CAP       cliente di fatturazione                                        17
038400040611T1  -  Località  cliente di fatturazione                                        18
038500040611T1  -  Provincia cliente di fatturazione                                        19
038600040611T2  -  Codice agente             (alfanumerico)                                 20
038700040611T2  -  Numero ordine             (alfanumerico)                                 21
038800040611T2  -  Data   ordine             (già editata)                                  22
038900040611T2  -  Riferimenti               (alfanumerico)                                 23
039000040611Codice cliente    NON numerico                                                  24
039100040611T2  -  Estremi documento         (alfanumerico)                                 25
039200040611T2  -  Data    documento         (già editata)                                  26
039300040611Numero pagina     NON numerico                                                  27
039400040611T2  -  Porto                                                                    28
039500040611Codice pagamento  NON numerico                                                  29
039600040611T2  -  Descrizione pagamento                                                    30
039700040611T2  -  Descrizione spedizioniere                                                31
039800040611C1  -  Codice prodotto           (alfanumerico)                                 32
039900040611C1  -  Descrizione prodotto      (alfanumerico)                                 33
040000040611C1  -  Quantità                  (già editata)                                  34
040100040611C2  -  Testo                                                                    35
040200040611C3  -  Testo                                                                    36
040300040611V1  -  Mezzo di trasporto                                                       37
040400040611V1  -  Causale trasporto                                                        38
040500040611V1  -  Numero colli               (già editato)                                 39
040600040611V1  -  Peso in kg.                (già editato)                                 40
040700040611V1  -  Aspetto esteriore dei beni ("Cartoni")                                   41
040800040611V2  -  Descrizione vettore                                                      42
040900040611V2  -  Indirizzo   vettore                                                      43
041000040611V2  -  Città       vettore                                                      44
041100040611V2  -  Data ritiro                                                              45
041200040611V2  -  Ora  ritiro                                                              46
041300040611V2  -  Annotazioni                                                              47
041400040611V2  -  Note vettore 1                                                           48
041500040611V2  -  Note vettore 2                                                           49
041600040611L1  -  Testo                                                                    50
041700040611O1  -  Testo                                                                    51
041800040611O2  -  Codice prodotto           (alfanumerico)                                 52
041900040611O2  -  Descrizione prodotto      (alfanumerico)                                 53
042000040611O2  -  Quantità                  (già editata)                                  54
