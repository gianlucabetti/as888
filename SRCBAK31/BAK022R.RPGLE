000100110531     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES)
000200110531     H*PARMS CVTOPT(*NONE)
000300110531     H DECEDIT('0,') DATEDIT(*DMY.)
000400110531     H*---------------------------------------------------------------*
000500110531     H* BAK022R                                                       *
000600110531     H*                                                               *
000700110531     H* INCREMENTA DI 1 IL NR. INCREMENTALE AZIONE SU KPDT1           *
000800110531X0309H*     o su KpNaz00F                                             *
000900110531     H*                                                               *
001000110531X0264H*                                                               *
001100110531  "  H* Se ricevuto in input il flag di errore (param. facoltativo)   *
001200110531  "  H*  vengono gestiti max 5 tentativi di allocazione della data    *
001300110531  "  H*  area KPDT1 dopodichè viene invioato un messagio di errore    *
001400110531  "  H*  alla coda dell'utente e dell'operatore                       *
001500110531  "  H* Se non ricevuto in input il flag di errore i tentativi di     *
001600110531  "  H*  allocazione della data area KPDT1 vengono ripetuti           *
001700110531X0264H*  teoricamente all'infinito                                    *
001800110531     H*                                                               *
001900110531     H*---------------------------------------------------------------*
002000110531X0309 * File numero azione
002100110531X0309FKpNaz00F  UF A E           K DISK    usropn
002200110531X0309FKpLog01L  IF A E           K DISK    usropn
002300110531      *-------------
002400110531     D KPDT1         E DS           512
002500110531X0309D KPDT0         E DS           100
002600110531X0264D*-------------
002700110531  "  D                SDS
002800110531  "  D  USER                 254    263
002900110531  "  D  JOBNUMA              264    269
003000110531  "  D  JOBNAM               244    253
003100110531  "  D  PARMS            *PARMS
003200110531  "  D*-------------
003300110531  "  D Errore022       S              1A
003400110531X0264D $Tentativi      S              2  0
003500110531X0309D*-------------
003600110531  "  D NrAzione        DS                  Inz
003700110531  "  D  DsSerie                1      1  0
003800110531  "  D  DsNr                   2      9  0
003900110531X0309D  DsNrAz                 1      9  0
004000110531     C*---------------------------------------------------------------*
004100110531      *
004200110531X0309X*** Riorganizzato in subroutines:
004300110531X0309X*** tutto quello che c'era è stato inserito nelle
004400110531X0309X*** subroutines inzvar, repkpdt1, endpgm
004500110531X0309X*** che al loro interno non sono siglate con x0309.
004600110531X0309X*** Le implementazioni rispetto allo stato ANTE-x0309
004700110531X0309X*** hanno mantenuto la sigla.
004800110531      *
004900110531X0309 * Operazioni iniziali
005000110531X0309C                   EXSR      Inzvar
005100110531      *
005200110531X0309 * Reperimento area dati kpdt0
005300110531 "    * per vedere come è gestito il numeratore
005400110531 "    * Per default il numeratore è in area dati KPDT1
005500110531 "   C                   Clear                   Kp0NSuFile        1
005600110531 "   C                   IN        KPDT0                                98
005700110531 "    *
005800110531 "    * Se trovo la dtara ed ho impostato il reperimento da file
005900110531 "    * Reperimento da KpAzn
006000110531 "   C                   If        Kp0NSuFile= *on
006100110531 "   C                   Exsr      RepKpNaz
006200110531X0309C                   Else
006300110531      *
006400110531X0309 * Reperimento numeratore da kpdt1
006500110531X0309C                   EXSR      RepKpDt1
006600110531X0309C                   Endif
006700110531      *
006800110531X0309 * fine programma
006900110531X0309C                   EXSR      Endpgm
007000110531      *
007100110531X0309C************************************************************
007200110531 "   C* Operazioni iniziali
007300110531 "   C************************************************************
007400110531X0309C     Inzvar        BEGSR
007500110531      *                                                                R
007600110531      *
007700110531     C     *ENTRY        PLIST
007800110531     C                   PARM                    WNRAZ             9 0
007900110531X0264C                   PARM                    Errore022
008000110531      *
008100110531     C     *DTAARA       DEFINE                  KPDT1
008200110531X0309C     *DTAARA       DEFINE                  KPDT0
008300110531      *
008400110531X0264C                   If        PARMS > 1
008500110531  "  C                   Eval      Errore022 = *Off
008600110531X0264C                   EndIf                                                  nr. azione
008700110531      *                                                                R
008800110531X0309C                   ENDSR
008900110531     C/EJECT
009000110531X0309C************************************************************
009100110531 "   C* Fine pgm
009200110531 "   C************************************************************
009300110531X0309C     ENDPGM        BEGSR
009400110531      *
009500110531     C                   SETON                                        LR
009600110531     C                   Return
009700110531      *
009800110531X0309C                   ENDSR
009900110531     C/EJECT
010000110531X0309C************************************************************
010100110531 "   C* Lettura KpNaz
010200110531 "   C************************************************************
010300110531X0309C     RepKpNaz      BEGSR
010400110531      *
010500110531      * Loop fino a quando trovo una azione che non sia già su
010600110531      * KpLog
010700110531      *
010800110531     C                   Do        *hival
010900110531      *
011000110531      * Tento il reperimento con la serie attuale
011100110531     C                   eval      NazSerie = Kp0SerieAt
011200110531     C                   clear                   NazNr
011300110531      * Reperimento file
011400110531      * ciclo per la serie, se trovo allocato la incremento
011500110531     C                   Exsr      CicloRep
011600110531      *
011700110531      * Preparo il nr azione da restituire
011800110531      * e aggiorno il db
011900110531      *   Incremento di 1 il numero trovato
012000110531      *   + aggiorno il file che ho letto e se necessario kpdt0
012100110531      *     con la serie di riferimento
012200110531     C                   Exsr      NewNrAz
012300110531      *
012400110531      * Nuovo nr azione
012500110531     C                   Z-ADD     NazSerie      DsSerie
012600110531     C                   Z-ADD     NazNr         DsNr
012700110531     C                   Move      DsNrAz        WNRAZ
012800110531      *
012900110531      * Non posso restituire il nr 0, nel caso lo incremento di 1
013000110531     C                   If        WNRAZ = 0
013100110531     C                   Exsr      NrAzZero
013200110531     C                   Endif
013300110531      *
013400110531      * Verifico che non sia già presente
013500110531     C                   Exsr      RepKpLog
013600110531     C                   If        $NewAZ = *on
013700110531     C                   Leave
013800110531     C                   Endif
013900110531     C                   EndDo
014000110531      *
014100110531X0309C                   ENDSR
014200110531X0309C************************************************************
014300110531 "   C* Preparo il nr azione da restituire
014400110531 "   C************************************************************
014500110531X0309C     NewNrAz       BEGSR
014600110531      *
014700110531      * Incremento di 1 il numero trovato
014800110531      * + aggiorno il file che ho letto e se necessario kpdt0
014900110531      * con la serie di riferimento
015000110531     C                   ADD       1             NazNr
015100110531      *
015200110531      * N.B. sto aggiornando la serie attiva
015300110531      * ------------------------------------
015400110531      * Record letto, lo aggiorno
015500110531     C                   If        $RcdL  = *on
015600110531     C                   Update(E) KpNaz000
015700110531     C                   Else
015800110531     C* record non letto, lo devo aggiungere
015900110531     C                   Write(E)  KpNaz000
016000110531     C                   Endif
016100110531      *
016200110531     C                   If        not %error
016300110531      *
016400110531      * Se il nr è andato a zero ho saturato la serie attiva
016500110531      * -----------------------------------------------------
016600110531      * Aggiornerò l'area dati KpDt0 con la NUOVA serie di riferimento
016700110531     C                   If        NazNR  = 0
016800110531     C                   Exsr      AggKpDt0
016900110531     C                   eval      NazSerie  = Kp0SerieAt
017000110531     C                   Endif
017100110531     C                   Endif
017200110531      *
017300110531X0309C                   ENDSR
017400110531X0309C************************************************************
017500110531 "   C* Ciclo Reperimento File
017600110531 "   C************************************************************
017700110531X0309C     CicloRep      BEGSR
017800110531      *
017900110531     C                   Do        *hival
018000110531     C                   Exsr      RepFile
018100110531      *
018200110531      * File non allocato
018300110531     C                   If        $rcdAlk = *Off
018400110531     C                   Leave
018500110531     C                   Else
018600110531      *
018700110531      * File allocato
018800110531      *
018900110531      * Incremento la serie:
019000110531      * se sono a 9 riparto da 0
019100110531     C                   If        NazSerie < 9
019200110531     C                   eval      NazSerie = NazSerie + 1
019300110531     C                   clear                   NazNr
019400110531     C                   Else
019500110531     C                   clear                   NazSerie
019600110531     C                   clear                   NazNr
019700110531     C                   Endif
019800110531     C                   Endif
019900110531      *
020000110531     C                   EndDo
020100110531      *
020200110531X0309C                   ENDSR
020300110531      *
020400110531     C************************************************************
020500110531     C* Non posso restituire il nr a zero
020600110531     C************************************************************
020700110531     C     NrAzZero      BEGSR
020800110531      *
020900110531     C                   Exsr      RepFile
021000110531      *
021100110531     C                   ADD       1             NazNr
021200110531      *
021300110531      * N.B. sto aggiornando la serie 0
021400110531      * In caso di serie 9 allocata NON passo alla serie 0
021500110531      * per cui non mi devo preoccupare di possibili azioni con serie 0
021600110531      * create in tempi recenti.
021700110531      * ------------------------------------
021800110531      * Record letto, lo aggiorno
021900110531     C                   If        $RcdL  = *on
022000110531     C                   Update(E) KpNaz000
022100110531     C                   Else
022200110531     C* record non letto, lo devo aggiungere
022300110531     C                   Write(E)  KpNaz000
022400110531     C                   Endif
022500110531      *
022600110531     C                   ENDSR
022700110531      *
022800110531X0309C************************************************************
022900110531 "   C* Reperimento File
023000110531 "   C************************************************************
023100110531X0309C     RepFile       BEGSR
023200110531      *
023300110531     C                   if        not %open(KpNaz00F)
023400110531     C                   Open      KpNaz00F
023500110531     C                   endif
023600110531      *
023700110531     C                   Move      *off          $rcdL             1
023800110531     C                   Move      *off          $rcdAlk           1
023900110531      *
024000110531     C     K01Naz00      Chain     Kpnaz00F                           2122
024100110531      *
024200110531     C                   If        *in22=*on
024300110531     C                   Eval      $rcdAlk = *On
024400110531     C                   Endif
024500110531      *
024600110531     C                   If        *in21=*off
024700110531     C                   Eval      $rcdL = *On
024800110531     C                   Endif
024900110531      *
025000110531      *
025100110531X0309C                   ENDSR
025200110531X0309C************************************************************
025300110531 "   C* Verifico che non ci sia già
025400110531 "   C************************************************************
025500110531X0309C     RepKpLog      BEGSR
025600110531      *
025700110531     c                   Move      *on           $NewAz            1
025800110531      *
025900110531     C                   if        not %open(KpLog01L)
026000110531     C                   Open      KpLog01L
026100110531     C                   endif
026200110531      *
026300110531     C     WnrAz         Chain     Kplog01L                           33
026400110531     C                   if        %found(KpLog01L)
026500110531     c                   Move      *off          $NewAz
026600110531     C                   endif
026700110531      *
026800110531X0309C                   ENDSR
026900110531X0309C************************************************************
027000110531 "   C* Lettura Kpdt0
027100110531 "   C************************************************************
027200110531X0309C     AggKpDt0      BEGSR
027300110531      *
027400110531     C     *IN99         DOUEQ     *OFF
027500110531     C     $Tentativi    OrEq      5
027600110531     C     PARMS         AndGT     1
027700110531     C     *LOCK         IN        KPDT0                                99
027800110531     C                   Eval      $Tentativi = $Tentativi + 1
027900110531     C                   ENDDO
028000110531      *
028100110531      * allocazione non riuscita
028200110531     C                   If        *In99 = *On
028300110531     C                   Eval      Errore022 = *On
028400110531     C                   Clear                   WNRAZ
028500110531     C* invio msg a utente e operatore
028600110531     C                   Eval      MsgDta024 = JOBNUMA + USER + JOBNAM
028700110531     C                   Call      'BAK024C'
028800110531     C                   Parm      '11'          Ope024            2
028900110531     C                   Parm      USER          User024          10
029000110531     C                   Parm      'PRO0045'     MsgId24           7
029100110531     C                   Parm      'PROBAS'      MsgF024          10
029200110531     C                   Parm                    MsgDta024       100
029300110531      * allocazione riuscita
029400110531     C                   Else
029500110531     C                   ADD       1             KP0SerieAT
029600110531     C                   OUT       KPDT0
029700110531     C                   EndIf
029800110531      *
029900110531      *
030000110531X0309C                   ENDSR
030100110531X0309C************************************************************
030200110531 "   C* Reperisce numero da dtara Kpdt1
030300110531 "   C************************************************************
030400110531X0309C     RepKpDt1      BEGSR
030500110531      *
030600110531     C*
030700110531X0264C                   Clear                   $Tentativi
030800110531     C     *IN99         DOUEQ     *OFF
030900110531X0264C     $Tentativi    OrEq      5
031000110531X0264C     PARMS         AndGT     1
031100110531     C     *LOCK         IN        KPDT1                                99
031200110531X0264C                   Eval      $Tentativi = $Tentativi + 1
031300110531     C                   ENDDO
031400110531X0264C* allocazione non riuscita
031500110531  "  C                   If        *In99 = *On
031600110531  "  C                   Eval      Errore022 = *On
031700110531  "  C                   Clear                   WNRAZ
031800110531  "  C* invio msg a utente e operatore
031900110531  "  C                   Eval      MsgDta024 = JOBNUMA + USER + JOBNAM
032000110531  "  C                   Call      'BAK024C'
032100110531  "  C                   Parm      '11'          Ope024            2
032200110531  "  C                   Parm      USER          User024          10
032300110531  "  C                   Parm      'PRO0045'     MsgId24           7
032400110531  "  C                   Parm      'PROBAS'      MsgF024          10
032500110531  "  C                   Parm                    MsgDta024       100
032600110531X0264C* allocazione riuscita
032700110531X0264C                   Else
032800110531     C*
032900110531     C                   ADD       1             KP1NRA
033000110531     C                   OUT       KPDT1
033100110531     C*
033200110531     C                   Z-ADD     KP1NRA        WNRAZ
033300110531X0264C                   EndIf
033400110531      *
033500110531X0309C                   ENDSR
033600110531     C/EJECT
033700110531X0309C************************************************************
033800110531 "   C* Definizioni
033900110531 "   C************************************************************
034000110531X0309C     Defcam        BEGSR
034100110531      *
034200110531      * Klist
034300110531     C* KpNaz00F
034400110531     C     K01Naz00      KLIST
034500110531     C                   KFLD                    NazSerie
034600110531      *
034700110531X0309C                   ENDSR
034800110531     C/EJECT
