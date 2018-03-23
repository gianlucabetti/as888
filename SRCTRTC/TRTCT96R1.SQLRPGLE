000100980618      ***********************************************************************
000200100811      *   Chiamato da TRTCT96R
000300960329      ***********************************************************************
000400040301 :    * Serve x trascodificare la tabella PT tramite l'UNB di codici dubbi
000500100628 :    * che gestiscono 2 nazioni
000600040301      ***********************************************************************
000700100628 :    * in questo modo posso prendere o dal primo dettaglio se soddisfa
000800100628 :    * l'aggancio dell'UNB oppure dal primo dettaglio valido successivo.
000900100628 :    * (Il NAD+CZ opportuno per poter decodificare la tabella PT.)
001000040301      ***********************************************************************
001100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
001200960329     H DECEDIT('0,') DATEDIT(*DMY.)
001300000120      *---------------------------------------------------------------------*
001400040302     FEDTAB01L  IF   E           K DISK
001500000120      *---------------------------------------------------------------------*
001600100628     D  Dati           s           2048
001700100628     D  VINDTA         s           2048
001800100628     D Tipo_seg        S              3
001900100720     D Trovato_seg     S              1
002000100716      *
002100100716     d keyUNBCLI       s             35
002200100716     d keyTIPOMSG      s              6
002300100716     d keyVERSION      s              3
002400100716     d keyRELEASE      s              3
002500100716     d keyAGENCY       s              3
002600100716     d keyASSOCIA      s              6
002700100716      *
002800100720     D DsGenerica      s           2048
002900100628     D segmento        s           2048
003000100628     D Errore          s              1
003100100628     D stringa_SQL     s           1024
003200100720???? D seg_imprevisto...
003300100720???? D                 s              1
003400100628      *---------------------------------------------------------------------*
003500000120      * .. per scompozione dati ricevuti a seconda del tipo record
003600100628      *---------------------------------------------------------------------*
003700100811???? D edS96UNB      E DS
003800100811???? D edS96NAD      E DS
003900100628      *---------------------------------------------------------------------*
004000100628     D Valori_inErr    ds
004100100628     D  SKout_Errori                  1    DIM(50)
004200100628     D Descr_Errore    ds
004300100628     D  SKout_DesErr                 50    DIM(50)
004400100628      *
004500100628     C*---------------------------------------------------------------*
004600100628      * Tabella partner
004700100628     C*---------------------------------------------------------------*
004800100628     D CPT             S             35    DIM(100)
004900100628     D  W035A          s             35
005000100628     D  almeno_uno     s              1
005100040310     C*---------------------------------------------------------------*
005200040310     D MsgDSP          S            256                                         Testo generico
005300040310     C*---------------------------------------------------------------*
005400040310     D SndMg01         S             10                                         Message type
005500040310     D                                     INZ('*INFO')
005600040310     D SndMg02         S             10                                         Deluvery mode
005700040310     D                                     INZ('*BREAK')
005800040310     D SndMg03         S            256                                         Message text
005900040310     D SndMg04         S             10I 0                                      Length of text
006000040310     D                                     INZ(%SIZE(SndMg03))
006100040310     D SndMg05         S             10                                         User profile
006200040310     D                                     DIM(299)
006300040310     D SndMg06         S             10I 0                                      Number of user
006400040310     D SndMg07         S             10I 0                                      Message sent indic.
006500040310     D SndMg08         S             10I 0                                      Function requested
006600040310     D SndMg10         S              1                                         Show display
006700040310     D                                     INZ('N')
006800040310     D SndMg11         S             20                                         Qualified MSGQ name
006900040310     D SndMg12         S              4                                         Name type indicator
007000040310     D                                     INZ('*USR')
007100040310      * indice di scihera
007200040310     D Jx              S              5I 0
007300040310      *
007400040310     D/COPY QSYSINC/QRPGLESRC,QUSEC
007500100628      *---------------------------------------------------------------------*
007600100628     d Legge_UNB_NADCZ...
007700100628     d                 C                   CONST('SELECT RRN(tivin00r), vindta -
007800100707     d                                         FROM tivin00r WHERE VINFLG='' ''-
007900100707     d                                          and ( VINDTA like ''UNB+%'' or -
008000100707     d                                         VINDTA like ''NAD+CZ%'')  -
008100100707     d                                          and RRN(tivin00r)>= ')
008200100707     d Ordina_SQL      c                   CONST(' ORDER BY RRN(tivin00r)')
008300100628     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400100628     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500000120      *---------------------------------------------------------------------*
008600000120      * Ciclo principale
008700000120      *---------------------------------------------------------------------*
008800040301      *  Parametrti ricevuti:
008900100628      *     Nrrec_a   = Num.Rel.Rec. Non serve più in questo programma
009000040301      *     Quale_Det = in quale dettaglio CZ reperire la nazione del mittente.
009100040301      *                 Solitamente lo si prende dal primo ma se ci fosse qualche
009200040301      *                 problema lo deve cercare sui successivi prendendo il primo
009300040301      *                 valido.
009400100811      *     Naz-Mitt  = è il parametro da restituire al TRTCT96R per permettere di
009500040301      *                 trovare l'UNB corretto.
009600040301      *---------------------------------------------------------------------*
009700040301     c     *entry        plist
009800040301     c                   parm                    Nrec              9 0
009900100812     c                   parm                    PT_Parziale      35
010000100812     c                   parm                    Naz_mitt          3
010100040302     c                   parm                    Ok_Naz            1
010200040322     c                   parm                    User_msg         10
010300100716     C                   parm                    keyUNBCLI
010400100716     C                   parm                    keyTIPOMSG
010500100716     C                   parm                    keyVERSION
010600100716     C                   parm                    keyRELEASE
010700100716     C                   parm                    keyAGENCY
010800100716     C                   parm                    keyASSOCIA
010900040302      *
011000040302      * Caricamento Tabella Partner esteri
011100040302     C                   Z-ADD     0             X                 3 0
011200040302     C                   MOVEL     'PT'          TABCOD
011300040302     C     TABCOD        CHAIN     EDTAB01L                           30
011400040302     C     *IN30         DOWEQ     '0'
011500040310     C     TABFLG        ifEQ      *BLANKS
011600040302     C                   ADD       1             X
011700040302     C                   MOVEL     TABKEY        CPT(X)
011800040302     C                   End
011900040302     C     TABCOD        READE     EDTAB01L                               30
012000040302     C                   END
012100040301     C*
012200040301     C* IMPOSTO A ZERO VARIABILI DI PGM
012300040301     C                   MOVEL     'N'           WFINE             1
012400040302     C                   MOVEL     'N'           OK_Naz            1
012500100812     C                   z-add     0             conta_NAD_CZ      5 0
012600100628     c                   z-add     0             NRelRec_VIN       9 0
012700040301      *
012800100628     c                   eval      stringa_SQL = Legge_UNB_NADCZ
012900100707     c                   eval      stringa_SQL = %trim(stringa_SQL) +
013000100707     c                             %trim(%editc(Nrec:'Z')) +
013100100707     c                             ordina_sql
013200100628      *
013300100628      * Lettura sequenziale solo dei segmenti (UNB) e (NAD+CZ)
013400100628     C/EXEC SQL
013500100628     C+ PREPARE Legge_TIVIN00R FROM :stringa_SQL
013600100628     C/END-EXEC
013700100628
013800100628     C/EXEC SQL
013900100628     C+ DECLARE Cursore_TIVIN CURSOR FOR Legge_TIVIN00R
014000100628     C/END-EXEC
014100100628
014200100628     C/EXEC SQL
014300100628     C+ OPEN Cursore_TIVIN
014400100628     C/END-EXEC
014500100628     c                   clear                   almeno_uno
014600100628
014700100628     C                   DOU       SqlCod <> 0
014800100628      *
014900100628      * Lettura sequenziale solo dei segmenti (UNB) e (NAD+CZ)
015000100628     C/EXEC SQL
015100100628     C+ FETCH NEXT FROM Cursore_TIVIN INTO :NRelRec_VIN, :VINDTA
015200100628     C/END-EXEC
015300100628     C                   SELECT
015400100628     c* a fine file
015500100628     C                   WHEN      SqlCod = 100  or Wfine = 'S'
015600100628     c                   leave
015700100628     **
015800100628     C                   WHEN      SqlCod < 0
015900100628     C                   seton                                        H2
016000100628     c                   leave
016100100628     c*
016200100628     C                   OTHER
016300100628     c* dettaglio
016400100628     c                   move      'S'           almeno_uno
016500100628      *
016600100628      ** Decodifica record a campi variabili
016700100628      * ?              /*---------------------- */
016800100628     c                   exsr      Decod_Segmento
016900100628      * ?              /*---------------------- */
017000100628      *
017100100628      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
017200100628     c                   if        OK_Naz = 'S'
017300100628     C                   leave
017400100628     C                   endif
017500100628      *
017600100628      * Se ha finito i dettaglio e legge una successiva testata senza avere
017700100628      *  trovato nulla deve uscire forzatamente
017800100812     c                   if        tipo_seg = 'UNB'  and conta_NAD_CZ >= 1
017900100628     C                   leave
018000100628     C                   endif
018100100628      **
018200100628     C                   ENDSL
018300100628      **
018400100628     C                   ENDDO
018500100628     C/EXEC SQL
018600100628     C+ CLOSE Cursore_TIVIN
018700100628     C/END-EXEC
018800040310      *
018900040310      * Msg di allerta Traduzione da Flat file non finita bene
019000040310     c                   IF        Ok_Naz = 'N'
019100040310     c                   exsr      SEND_MSG_EDP
019200040310     C                   END                                                    - Fine file -
019300040310      *
019400960329     C                   SETON                                        LR
019500100628      * ?------------------------------------------------------------------ */
019600100628      *?    Decodifica il segmento ed importa i campi in formato DS
019700100628      * ?------------------------------------------------------------------ */
019800100628     c     Decod_SegmentoBegsr
019900100628      **
020000100628      *  imposta il tipo di segmento:
020100100628     c                   clear                   dati
020200100628     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
020300100628     c                   eval      tipo_seg = %subst(dati:1:3)
020400100628     c                   eval      segmento = dati
020500100720      *
020600100720     c                   clear                   seg_imprevisto
020700100720     c                   clear                   trovato_seg
020800100720      *
020900100720      *  Chiamata generica x decodificare il singolo segmento letto
021000100720     c                   call      'TRTCT00R1'
021100100720      * input
021200100720     c                   parm                    tipo_seg
021300100720     c                   parm                    segmento
021400100720     C                   parm                    keyUNBCLI
021500100811     C                   parm                    keyTIPOMSG
021600100811     C                   parm                    keyVERSION
021700100811     C                   parm                    keyRELEASE
021800100811     C                   parm                    keyAGENCY
021900100720     C                   parm                    keyASSOCIA
022000100720      *output
022100100720     c                   parm                    Errore
022200100720     c                   parm                    DsGenerica
022300100720     c                   parm                    Valori_inErr
022400100720     c                   parm                    Descr_Errore
022500100720     c                   parm                    trovato_seg
022600100720      **--------
022700100720      *  Ritorna la DS GENERICA oppure l'errore
022800100628      **--------
022900100628      *  Contatore delle righe ossia dei segmenti del messaggio
023000100628     c                   add       1             Conta_segmenti    5 0
023100100628      **--------
023200100628     c                   select
023300100628      **--------
023400100628     c                   when      tipo_seg = 'UNB'
023500100811     c                   movel(p)  DsGenerica    EDS96UNB
023600100628      *
023700100628      **--------
023800100628     c                   when      tipo_seg = 'NAD'
023900100811     c                   movel(p)  DsGenerica    EDS96NAD
024000100628     C*
024100100628     C* Se il primo record letto non è la testata Partner UNB
024200100628     C*  deve uscire dal pgm senza dare nessun esito
024300100812     c                   If          PT_Parziale = *blank
024400100628     c                   move      'S'           Wfine                          --> Esce in Errore
024500100628     c                   ELSE
024600100628      *
024700100628      * Dati mittente  con la NAZIONE mittente:
024800100628     C                   if         NAD3207 <> *blanks
024900100812     C                   add       1             conta_NAD_CZ
025000100628      * Verifica esistenza su tab.:PT
025100100812     C                   eval      W035A = %subst(PT_Parziale:1:31) + NAD3207
025200100628     C                   z-add     1             X
025300100628     C     W035a         lookup    CPT(X)                                 32
025400100628     C                   If        *in32 = *on
025500100628     c                   movel     NAD3207       Naz_mitt                       --> Decodificata
025600100628     C                   movel     'S'           OK_Naz                             La NAZIONE
025700100628     c                   endIF
025800100628     c                   end
025900100628      *
026000100628     C                   ENDIF
026100100628      **--------
026200100628     c                   endSL
026300100628      **
026400100628     c     end_Decod     endSR
026500040310     c*==================================================================*
026600040310      * Manda un Msg a EDP*
026700040310     c*==================================================================*
026800040310     c     SEND_MSG_EDP  begsr
026900040310      *
027000040310      * Imposta la Frase del messaggio
027100040310     c                   clear                   msgDSP
027200100811     C                   EVAL      msgDSP = 'ATTENZIONE: TRTCT96R1 non riesce a-
027300041207     C                              decod. il Cod.Partner da Tab.(PT). Il mitte-
027400041207     C                             nte ricevuto nell''IFCSUM è >'
027500100812     C                   EVAL      msgDSP  = %Trim(msgDSP) + PT_Parziale +
027600100628     C                             %Trim('< Controllare il Messaggio Ricevuto>')
027700040310      *
027800040310      * INVIA MESSAGGIO ad un Utente --> EDPAB
027900040322     c                   if        User_msg <> *blank
028000040310     c                   z-add     1             Jx
028100040310     C                   exsr      CalQEZSNDMG
028200040322     c                   end
028300040310      *
028400040310     c                   endsr
028500040310     ***********************************************************************
028600040310     **
028700040310     ** Send Message (QEZSNDMG) API
028800040310     **
028900040310     ***********************************************************************
029000040310     C     CalQEZSNDMG   BEGSR
029100040310      *
029200040310     ** Invio messaggio all'utente.
029300040310     C                   EVAL      SndMg03 = msgDSP
029400040322     C******             EVAL      SndMg05(Jx) = 'EDPAB     '
029500040322     C                   EVAL      SndMg05(Jx) = User_msg
029600040310     C                   EVAL      SndMg06 = Jx
029700040310     C                   CLEAR                   QUSEC
029800040310     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029900040310
030000040310     C                   CALL      'QEZSNDMG'
030100040310     C                   PARM                    SndMg01
030200040310     C                   PARM                    SndMg02
030300040310     C                   PARM                    SndMg03
030400040310     C                   PARM                    SndMg04
030500040310     C                   PARM                    SndMg05
030600040310     C                   PARM                    SndMg06
030700040310     C                   PARM                    SndMg07
030800040310     C                   PARM                    SndMg08
030900040310     C                   PARM                    Qusec
031000040310     C                   PARM                    SndMg10
031100040310     C                   PARM                    SndMg11
031200040310     C                   PARM                    SndMg12
031300040310      *
031400040310     C                   ENDSR
031500040310     O*****************************************************************
