000100010517     H DECEDIT('0,') DATEDIT(*YMD.)
000200010517
000300010517      **************************************************************************
000400010517      *                                                                        *
000500010517      *          Pulizia e/o Trasmissione potenziali doppi in sede             *
000600010517      *                                                                        *
000700010517      *                                                                        *
000800010517      **************************************************************************
000900010517
001000010517     FPOTEN00F  IF   E             DISK    RENAME(poten00f:poten)
001100010517     F                                     PREFIX(P_)
001200010517     FTNCPO01L  UF   E           K DISK
001300010517     FTNCPS11L  IF   E           K DISK
001400010517     FAZORG01L  IF   E           K DISK
001500010517     FQSYSPRT   O    f  132        PRINTER oflind(*inof)
001600010517
001700010517     D noazioni        s              1
001800010517     D W0030           s              3  0
001900010518     D fel_1           s                   like(orgfel)                         AS 1° potenziale
002000010518     D fel_2           s                   like(orgfel)                         AS 2° potenziale
002100010517     D A_fel           s                   like(orgfel)                         AS azione
002200010517     D P_fel           s                   like(orgfel)                         AS potenziale
002300010517     D P_DAQ           s                   like(CPODAQ)                         AS potenziale
002400010517
002500010517      *
002600010517      *--------------------------------------------------------------------------------------------
002700010517      * se ha data acquisizione diversa da 20010101 vuol dire che non ho
002800010517      * ricevuto aggiornamenti dalla filiale
002900010517      * devo verificare se ha delle azioni
003000010517      * A) ha azioni con AS uguale del commerciale azione e AS di
003100010517      *    trasmissione del nuovo record deleto il record trasmesso dalla
003200010517      *    sede e slaggo quello ricevuto dalla filiale
003300010517      * B) ha azioni con AS differenti tra commerciale azione e AS di
003400010517      *    trasmissione annullo il record trasmesso dalla sede e sflaggo
003500010517      * C) non ha azioni stampo il record
003600010517      *
003700010517      * se ha data acquisizione uguale a 20010101 e l'ho ricevuto dalla
003800010517      * filiale dopo il 20010216 vuol dire che ho ricevuto un aggiornamento
003900010517      * dalla filale (giusto oppure no)
004000010517      * devo verificare se ha azioni
004100010517      * A) ha azioni con AS uguale del commerciale azione e AS di trasmissio
004200010517      *    ne del nuovo record deleto il record trasmesso dalla sede
004300010517      * B) ha azioni con AS diverso del commerciale azione e AS di trasmissio
004400010517      *    ne annullo il record e lo trasmetto (così trasmetto l'annullamento
004500010517      *    nella nuova filiale e riceverò dalla filiale buona il record giusto)
004600010517      * C) non ha azioni lo stampo
004700010518      *
004800010518      * i senza azioni se sono dello stesso AS deleto il vecchio
004900010518      *                se non hanno lo stesso AS annullo e sflaggo quello inviato dalla sede
005000010517      *--------------------------------------------------------------------------------------------
005100010517      *
005200010517      *    Lettura File Potenziale di comodo
005300010517      *    =================================
005400010517
005500010517     C                   do        *hival
005600010517     C                   read      POTEN00F
005700010517     C                   if        %eof(POTEN00F)
005800010517     C                   leave
005900010517     C                   endif
006000010517      *
006100010517     c                   clear                   A_fel
006200010517     c                   clear                   P_fel
006300010518     c                   clear                   fel_1
006400010518     c                   clear                   fel_2
006500010517     c                   clear                   noazioni
006600010518      *
006700010518     c     P_cpoflt      chain     azorg01l
006800010518     c                   if        not %found(azorg01l) or orgfva <> ' '
006900010518     c                   iter
007000010518     c                   else
007100010518      * salvo AS
007200010518     c                   eval      fel_1 = orgfel
007300010518     c                   endif
007400010517      *
007500010517     C                   eval      P_DAQ = P_CPODAQ
007600010517      *
007700010517      * leggo il record successivo per recuperare l'AS
007800010517     c                   read      POTEN00f
007900010517     c                   if        not %eof(POTEN00F)
008000010517     c     P_cpoflt      chain     azorg01l
008100010517     c                   if        not %found(azorg01l) or orgfva <> ' '
008200010517     c                   iter
008300010517     c                   else
008400010517      * salvo AS
008500010517     c                   eval      P_fel = orgfel
008600010518     c                   eval      fel_2 = orgfel
008700010517     c                   endif
008800010517     c                   else
008900010517     c                   leave
009000010517     c                   endif
009100010517      *
009200010517      * recupero le azioni commerciali
009300010517     c                   exsr      REC_CPS
009400010517      * se non ci sono azioni stampo
009500010517     c                   if        noazioni = 'S'
009600010517     c                   if        *inof or not *in99
009700010517     c                   except    testa
009800010517     c                   setoff                                       of
009900010517     c                   seton                                        99
010000010517     c                   endif
010100010517     c                   except    detta
010200010518      * pulizia dei senza azioni
010300010518     c                   EXSR      senza
010400010517     c                   iter
010500010517     c                   endif
010600010517      *
010700010517      * controllo data acquisizione
010800010517     c                   if        P_daq  <> 20010101
010900010517      *
011000010517     C                   EXSR      VECCHIO
011100010517      *
011200010517     c                   else
011300010517      *
011400010517     C                   EXSR      NUOVO
011500010517      *
011600010517     c                   endif
011700010517      *
011800010517     c                   enddo
011900010517      *
012000010517     c                   seton                                        lr
012100010517
012200010517      *---------------------------------------------------------------------------------------------
012300010517      *              GESTIONE POTENZIALE CON DATA ACQUISIZIONE DIVERSA DA 20010101
012400010517      *---------------------------------------------------------------------------------------------
012500010517      *
012600010517     C     VECCHIO       BEGSR
012700010517      *
012800010517      * verifico AS della trasmissione nuova acquisizione con AS dell'azione commerciale
012900010517      *
013000010517      * DIVERSI
013100010517      *
013200010517     c                   if        P_fel <> A_fel
013300010517      * aggiorno il record inviato dalla sede
013400010517     c     P_cpocpo      setll     tncpo01l
013500010517     c                   do        *hival
013600010517     c     P_cpocpo      reade     tncpo01l
013700010517     c                   if        not %eof(tncpo01l)
013800010517      * verifico la data trasmissione
013900010517     c                   if        cpoftr = 'T'  and cpodtr = 20010216
014000010517      * se è il record che abbiamo trasmesso noi dalla sede annullo e sflaggo per la trasmissione
014100010517     c                   eval      cpoatb = 'A'
014200010517     c                   clear                   cpoftr
014300010517     c                   update    tncpo000
014400010517     c                   leave
014500010517     c                   else
014600010517     c                   unlock    tncpo01l
014700010517     c                   iter
014800010517     c                   endif
014900010517     c                   else
015000010517     c                   leave
015100010517     c                   endif
015200010517     c                   enddo
015300010517      *
015400010517      * UGUALI
015500010517      *
015600010517     c                   else
015700010517      * aggiorno il record inviato dalla sede
015800010517     c     P_cpocpo      setll     tncpo01l
015900010517     c                   do        *hival
016000010517     c     P_cpocpo      reade     tncpo01l
016100010517     c                   if        not %eof(tncpo01l)
016200010517      * verifico la data trasmissione
016300010517     c                   if        cpoftr <> 'T'  or  cpodtr <> 20010216
016400010517      * se è il record che abbiamo trasmesso noi dalla sede ma vecchio oppure ricevuto dalla filiale
016500010517      * lo sflaggo per la trasmissione
016600010517     c                   clear                   cpoftr
016700010517     c                   update    tncpo000
016800010517     c                   iter
016900010517     c                   endif
017000010517      * verifico la data trasmissione
017100010517     c                   if        cpoftr = 'T'  and cpodtr =  20010216
017200010517      * se è il record che abbiamo trasmesso noi dalla sede il 20010216
017300010517      * lo deleto
017400010517     c                   delete    tncpo000
017500010517     c                   iter
017600010517     c                   endif
017700010517     c                   else
017800010517     c                   leave
017900010517     c                   endif
018000010517     c                   enddo
018100010517      *
018200010517     c                   endif
018300010517      *
018400010517     c                   ENDSR
018500010517
018600010517      *---------------------------------------------------------------------------------------------
018700010517      *              GESTIONE POTENZIALE CON DATA ACQUISIZIONE UGUALE A 20010101
018800010517      *---------------------------------------------------------------------------------------------
018900010517      *
019000010517     C     NUOVO         BEGSR
019100010517      *
019200010517      * verifico AS della trasmissione nuova acquisizione con AS dell'azione commerciale
019300010517      *
019400010517      * DIVERSI
019500010517      *
019600010517     c                   if        P_fel <> A_fel
019700010517      * aggiorno il record inviato dalla sede
019800010517     c     P_cpocpo      setll     tncpo01l
019900010517     c                   do        *hival
020000010517     c     P_cpocpo      reade     tncpo01l
020100010517     c                   if        not %eof(tncpo01l)
020200010517      * verifico la data trasmissione
020300010517     c                   if        cpoftr = 'T'  and cpodtr = 20010216
020400010517      * se è il record che abbiamo trasmesso noi dalla sede annullo e sflaggo per la trasmissione
020500010517     c                   eval      cpoatb = 'A'
020600010517     c                   clear                   cpoftr
020700010517     c                   update    tncpo000
020800010517     c                   leave
020900010517     c                   else
021000010517     c                   unlock    tncpo01l
021100010517     c                   iter
021200010517     c                   endif
021300010517     c                   else
021400010517     c                   leave
021500010517     c                   endif
021600010517     c                   enddo
021700010517      *
021800010517      *
021900010517      * UGUALI
022000010517      *
022100010517     c                   else
022200010517      * aggiorno il record inviato dalla sede
022300010517     c     P_cpocpo      setll     tncpo01l
022400010517     c                   do        *hival
022500010517     c     P_cpocpo      reade     tncpo01l
022600010517     c                   if        not %eof(tncpo01l)
022700010517      * verifico la data trasmissione
022800010517     c                   if        cpoftr = 'T'  and cpodtr =  20010216
022900010517      * se è il record che abbiamo trasmesso noi dalla sede il 20010216
023000010517      * lo deleto
023100010517     c                   delete    tncpo000
023200010517     c                   iter
023300010517     c                   else
023400010517     c                   unlock    tncpo01l
023500010518     c                   iter
023600010517     c                   endif
023700010517     c                   else
023800010517     c                   leave
023900010517     c                   endif
024000010517     c                   enddo
024100010517      *
024200010517     c                   endif
024300010517      *
024400010517     c                   ENDSR
024500010517
024600010518      *---------------------------------------------------------------------------------------------
024700010518      *              SENZA AZIONI
024800010518      *---------------------------------------------------------------------------------------------
024900010518      *
025000010518     C     SENZA         BEGSR
025100010518      *
025200010518      * verifico AS dei due POTENZIALI
025300010518      *
025400010518      * DIVERSI
025500010518      *
025600010518     c                   if        fel_1 <> fel_2
025700010518      * aggiorno il record inviato dalla sede
025800010518     c     P_cpocpo      setll     tncpo01l
025900010518     c                   do        *hival
026000010518     c     P_cpocpo      reade     tncpo01l
026100010518     c                   if        not %eof(tncpo01l)
026200010518      * verifico la data trasmissione
026300010518     c                   if        cpoftr = 'T'  and cpodtr = 20010216
026400010518      * se è il record che abbiamo trasmesso noi dalla sede annullo e sflaggo per la trasmissione
026500010518     c                   eval      cpoatb = 'A'
026600010518     c                   clear                   cpoftr
026700010518     c                   update    tncpo000
026800010518     c                   leave
026900010518     c                   else
027000010518     c                   unlock    tncpo01l
027100010518     c                   iter
027200010518     c                   endif
027300010518     c                   else
027400010518     c                   leave
027500010518     c                   endif
027600010518     c                   enddo
027700010518      *
027800010518      * UGUALI
027900010518      *
028000010518     c                   else
028100010518      * aggiorno il record inviato dalla sede
028200010518     c     P_cpocpo      setll     tncpo01l
028300010518     c                   do        *hival
028400010518     c     P_cpocpo      reade     tncpo01l
028500010518     c                   if        not %eof(tncpo01l)
028600010518      * verifico la data trasmissione
028700010518     c                   if        cpoftr <> 'T'  or  cpodtr <> 20010216
028800010518      * se è il record che abbiamo trasmesso noi dalla sede ma vecchio oppure ricevuto dalla filiale
028900010518      * lo deleto
029000010518     c                   delete    tncpo000
029100010518     c                   leave
029200010518     c                   endif
029300010518     c                   else
029400010518     c                   leave
029500010518     c                   endif
029600010518     c                   enddo
029700010518      *
029800010518     c                   endif
029900010518      *
030000010518     c                   ENDSR
030100010517      *---------------------------------------------------------------------------------------------
030200010517      *                    RECUPERO AS AZIONI COMMERCIALE
030300010517      *---------------------------------------------------------------------------------------------
030400010517      *
030500010517     c     REC_CPS       BEGSR
030600010517      *
030700010517     c                   clear                   noazioni
030800010518     c     P_cpocpo      setll     TNCPS11L                               30
030900010517      *
031000010517      * se non ci sono azioni
031100010518     c                   if        not *in30
031200010517     c                   eval      noazioni = 'S'
031300010517     c                   endif
031400010517      * se ci sono azioni leggo
031500010517     c                   if        noazioni = ' '
031600010517     c                   DO        *HIVAL
031700010517     c     P_cpocpo      reade     TNCPS11l
031800010517      *
031900010517     c                   if        %eof(TNCPS11L)
032000010517     c                   leave
032100010517     c                   endif
032200010517      *
032300010517      * cerco l'as di appartenenza del P.O. commerciale
032400010517     c                   movel     CPSCMM        W0030
032500010517     c     W0030         chain     azorg01l
032600010517     c                   if        not %found(azorg01l) or orgfva <> ' '
032700010517     c                   iter
032800010517     c                   else
032900010518      * salvo AS
033000010517     c                   eval      A_fel = orgfel
033100010517     c                   endif
033200010517      * se as differente dall'as del potenziale vado a fine routine
033300010517      * altrimenti leggo l'azione precedente
033400010517     c                   if        A_fel <> P_fel and A_fel <> *zeros
033500010517     c                   leave
033600010517     c                   else
033700010517     c                   iter
033800010517     c                   endif
033900010517      *
034000010517     c                   enddo
034100010517      * se non trovate valorizzo l'AS con quello del potenziale
034200010517     c                   if        A_fel = *zeros
034300010517     c                   eval      A_fel =  P_fel
034400010517     c                   endif
034500010517      *
034600010517     c                   endif
034700010517      *
034800010517     c                   ENDSR
034900010517     Oqsysprt   E            TESTA             2
035000010517     O                                           70 ' ELENCO POTENZIALI NON    '
035100010517     O                                              'AGGIORNATI '
035200010517     O                                          125 'PAG.'
035300010517     O                       PAGE          Z    132
035400010517     O          E            testa       1
035500010517     O                                              'Cod.Potenziale'
035600010517     O          E            detta       1
035700010517     O                       P_cpocpo      z  +   2
