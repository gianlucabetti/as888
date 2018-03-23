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
001100960329     H DECEDIT('0,') DATEDIT(*DMY.)
001200000120      *---------------------------------------------------------------------*
001300040302     FEDTAB01L  IF   E           K DISK
001400000120      *---------------------------------------------------------------------*
001500100628     D  Dati           s           2048
001600100628     D  VINDTA         s           2048
001700100628     D Tipo_seg        S              3
001800100720     D Trovato_seg     S              1
001900100716      *
002000100716     d keyUNBCLI       s             35
002100100716     d keyTIPOMSG      s              6
002200100716     d keyVERSION      s              3
002300100716     d keyRELEASE      s              3
002400100716     d keyAGENCY       s              3
002500100716     d keyASSOCIA      s              6
002600100716      *
002700100720     D DsGenerica      s           2048
002800100628     D segmento        s           2048
002900100628     D Errore          s              1
003000100628     D stringa_SQL     s           1024
003100100720???? D seg_imprevisto...
003200100720???? D                 s              1
003300100628      *---------------------------------------------------------------------*
003400000120      * .. per scompozione dati ricevuti a seconda del tipo record
003500100628      *---------------------------------------------------------------------*
003600100811???? D edS96UNB      E DS
003700100811???? D edS96NAD      E DS
003800100628      *---------------------------------------------------------------------*
003900100628     D Valori_inErr    ds
004000100628     D  SKout_Errori                  1    DIM(50)
004100100628     D Descr_Errore    ds
004200100628     D  SKout_DesErr                 50    DIM(50)
004300100628      *
004400100628     C*---------------------------------------------------------------*
004500100628      * Tabella partner
004600100628     C*---------------------------------------------------------------*
004700100628     D CPT             S             35    DIM(100)
004800100628     D  W035A          s             35
004900100628     D  almeno_uno     s              1
005000040310     C*---------------------------------------------------------------*
005100040310     D MsgDSP          S            256                                         Testo generico
005200040310     C*---------------------------------------------------------------*
005300040310     D SndMg01         S             10                                         Message type
005400040310     D                                     INZ('*INFO')
005500040310     D SndMg02         S             10                                         Deluvery mode
005600040310     D                                     INZ('*BREAK')
005700040310     D SndMg03         S            256                                         Message text
005800040310     D SndMg04         S             10I 0                                      Length of text
005900040310     D                                     INZ(%SIZE(SndMg03))
006000040310     D SndMg05         S             10                                         User profile
006100040310     D                                     DIM(299)
006200040310     D SndMg06         S             10I 0                                      Number of user
006300040310     D SndMg07         S             10I 0                                      Message sent indic.
006400040310     D SndMg08         S             10I 0                                      Function requested
006500040310     D SndMg10         S              1                                         Show display
006600040310     D                                     INZ('N')
006700040310     D SndMg11         S             20                                         Qualified MSGQ name
006800040310     D SndMg12         S              4                                         Name type indicator
006900040310     D                                     INZ('*USR')
007000040310      * indice di scihera
007100040310     D Jx              S              5I 0
007200040310      *
007300040310     D/COPY QSYSINC/QRPGLESRC,QUSEC
007400100628      *---------------------------------------------------------------------*
007500100628     d Legge_UNB_NADCZ...
007600100628     d                 C                   CONST('SELECT RRN(tivin00r), vindta -
007700100707     d                                         FROM tivin00r WHERE VINFLG='' ''-
007800100707     d                                          and ( VINDTA like ''UNB+%'' or -
007900100707     d                                         VINDTA like ''NAD+CZ%'')  -
008000100707     d                                          and RRN(tivin00r)>= ')
008100100707     d Ordina_SQL      c                   CONST(' ORDER BY RRN(tivin00r)')
008200100628     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300100628     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400000120      *---------------------------------------------------------------------*
008500000120      * Ciclo principale
008600000120      *---------------------------------------------------------------------*
008700040301      *  Parametrti ricevuti:
008800100628      *     Nrrec_a   = Num.Rel.Rec. Non serve più in questo programma
008900040301      *     Quale_Det = in quale dettaglio CZ reperire la nazione del mittente.
009000040301      *                 Solitamente lo si prende dal primo ma se ci fosse qualche
009100040301      *                 problema lo deve cercare sui successivi prendendo il primo
009200040301      *                 valido.
009300100811      *     Naz-Mitt  = è il parametro da restituire al TRTCT96R per permettere di
009400040301      *                 trovare l'UNB corretto.
009500040301      *---------------------------------------------------------------------*
009600040301     c     *entry        plist
009700040301     c                   parm                    Nrec              9 0
009800100812     c                   parm                    PT_Parziale      35
009900100812     c                   parm                    Naz_mitt          3
010000040302     c                   parm                    Ok_Naz            1
010100040322     c                   parm                    User_msg         10
010200100716     C                   parm                    keyUNBCLI
010300100716     C                   parm                    keyTIPOMSG
010400100716     C                   parm                    keyVERSION
010500100716     C                   parm                    keyRELEASE
010600100716     C                   parm                    keyAGENCY
010700100716     C                   parm                    keyASSOCIA
010800040302      *
010900040302      * Caricamento Tabella Partner esteri
011000040302     C                   Z-ADD     0             X                 3 0
011100040302     C                   MOVEL     'PT'          TABCOD
011200040302     C     TABCOD        CHAIN     EDTAB01L                           30
011300040302     C     *IN30         DOWEQ     '0'
011400040310     C     TABFLG        ifEQ      *BLANKS
011500040302     C                   ADD       1             X
011600040302     C                   MOVEL     TABKEY        CPT(X)
011700040302     C                   End
011800040302     C     TABCOD        READE     EDTAB01L                               30
011900040302     C                   END
012000040301     C*
012100040301     C* IMPOSTO A ZERO VARIABILI DI PGM
012200040301     C                   MOVEL     'N'           WFINE             1
012300040302     C                   MOVEL     'N'           OK_Naz            1
012400100812     C                   z-add     0             conta_NAD_CZ      5 0
012500100628     c                   z-add     0             NRelRec_VIN       9 0
012600040301      *
012700100628     c                   eval      stringa_SQL = Legge_UNB_NADCZ
012800100707     c                   eval      stringa_SQL = %trim(stringa_SQL) +
012900100707     c                             %trim(%editc(Nrec:'Z')) +
013000100707     c                             ordina_sql
013100100628      *
013200100628      * Lettura sequenziale solo dei segmenti (UNB) e (NAD+CZ)
013300100628     C/EXEC SQL
013400100628     C+ PREPARE Legge_TIVIN00R FROM :stringa_SQL
013500100628     C/END-EXEC
013600100628
013700100628     C/EXEC SQL
013800100628     C+ DECLARE Cursore_TIVIN CURSOR FOR Legge_TIVIN00R
013900100628     C/END-EXEC
014000100628
014100100628     C/EXEC SQL
014200100628     C+ OPEN Cursore_TIVIN
014300100628     C/END-EXEC
014400100628     c                   clear                   almeno_uno
014500100628
014600100628     C                   DOU       SqlCod <> 0
014700100628      *
014800100628      * Lettura sequenziale solo dei segmenti (UNB) e (NAD+CZ)
014900100628     C/EXEC SQL
015000100628     C+ FETCH NEXT FROM Cursore_TIVIN INTO :NRelRec_VIN, :VINDTA
015100100628     C/END-EXEC
015200100628     C                   SELECT
015300100628     c* a fine file
015400100628     C                   WHEN      SqlCod = 100  or Wfine = 'S'
015500100628     c                   leave
015600100628     **
015700100628     C                   WHEN      SqlCod < 0
015800100628     C                   seton                                        H2
015900100628     c                   leave
016000100628     c*
016100100628     C                   OTHER
016200100628     c* dettaglio
016300100628     c                   move      'S'           almeno_uno
016400100628      *
016500100628      ** Decodifica record a campi variabili
016600100628      * ?              /*---------------------- */
016700100628     c                   exsr      Decod_Segmento
016800100628      * ?              /*---------------------- */
016900100628      *
017000100628      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
017100100628     c                   if        OK_Naz = 'S'
017200100628     C                   leave
017300100628     C                   endif
017400100628      *
017500100628      * Se ha finito i dettaglio e legge una successiva testata senza avere
017600100628      *  trovato nulla deve uscire forzatamente
017700100812     c                   if        tipo_seg = 'UNB'  and conta_NAD_CZ >= 1
017800100628     C                   leave
017900100628     C                   endif
018000100628      **
018100100628     C                   ENDSL
018200100628      **
018300100628     C                   ENDDO
018400100628     C/EXEC SQL
018500100628     C+ CLOSE Cursore_TIVIN
018600100628     C/END-EXEC
018700040310      *
018800040310      * Msg di allerta Traduzione da Flat file non finita bene
018900040310     c                   IF        Ok_Naz = 'N'
019000040310     c                   exsr      SEND_MSG_EDP
019100040310     C                   END                                                    - Fine file -
019200040310      *
019300960329     C                   SETON                                        LR
019400100628      * ?------------------------------------------------------------------ */
019500100628      *?    Decodifica il segmento ed importa i campi in formato DS
019600100628      * ?------------------------------------------------------------------ */
019700100628     c     Decod_SegmentoBegsr
019800100628      **
019900100628      *  imposta il tipo di segmento:
020000100628     c                   clear                   dati
020100100628     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
020200100628     c                   eval      tipo_seg = %subst(dati:1:3)
020300100628     c                   eval      segmento = dati
020400100720      *
020500100720     c                   clear                   seg_imprevisto
020600100720     c                   clear                   trovato_seg
020700100720      *
020800100720      *  Chiamata generica x decodificare il singolo segmento letto
020900100720     c                   call      'TRTCT00R1'
021000100720      * input
021100100720     c                   parm                    tipo_seg
021200100720     c                   parm                    segmento
021300100720     C                   parm                    keyUNBCLI
021400100811     C                   parm                    keyTIPOMSG
021500100811     C                   parm                    keyVERSION
021600100811     C                   parm                    keyRELEASE
021700100811     C                   parm                    keyAGENCY
021800100720     C                   parm                    keyASSOCIA
021900100720      *output
022000100720     c                   parm                    Errore
022100100720     c                   parm                    DsGenerica
022200100720     c                   parm                    Valori_inErr
022300100720     c                   parm                    Descr_Errore
022400100720     c                   parm                    trovato_seg
022500100720      **--------
022600100720      *  Ritorna la DS GENERICA oppure l'errore
022700100628      **--------
022800100628      *  Contatore delle righe ossia dei segmenti del messaggio
022900100628     c                   add       1             Conta_segmenti    5 0
023000100628      **--------
023100100628     c                   select
023200100628      **--------
023300100628     c                   when      tipo_seg = 'UNB'
023400100811     c                   movel(p)  DsGenerica    EDS96UNB
023500100628      *
023600100628      **--------
023700100628     c                   when      tipo_seg = 'NAD'
023800100811     c                   movel(p)  DsGenerica    EDS96NAD
023900100628     C*
024000100628     C* Se il primo record letto non è la testata Partner UNB
024100100628     C*  deve uscire dal pgm senza dare nessun esito
024200100812     c                   If          PT_Parziale = *blank
024300100628     c                   move      'S'           Wfine                          --> Esce in Errore
024400100628     c                   ELSE
024500100628      *
024600100628      * Dati mittente  con la NAZIONE mittente:
024700100628     C                   if         NAD3207 <> *blanks
024800100812     C                   add       1             conta_NAD_CZ
024900100628      * Verifica esistenza su tab.:PT
025000100812     C                   eval      W035A = %subst(PT_Parziale:1:31) + NAD3207
025100100628     C                   z-add     1             X
025200100628     C     W035a         lookup    CPT(X)                                 32
025300100628     C                   If        *in32 = *on
025400100628     c                   movel     NAD3207       Naz_mitt                       --> Decodificata
025500100628     C                   movel     'S'           OK_Naz                             La NAZIONE
025600100628     c                   endIF
025700100628     c                   end
025800100628      *
025900100628     C                   ENDIF
026000100628      **--------
026100100628     c                   endSL
026200100628      **
026300100628     c     end_Decod     endSR
026400040310     c*==================================================================*
026500040310      * Manda un Msg a EDP*
026600040310     c*==================================================================*
026700040310     c     SEND_MSG_EDP  begsr
026800040310      *
026900040310      * Imposta la Frase del messaggio
027000040310     c                   clear                   msgDSP
027100100811     C                   EVAL      msgDSP = 'ATTENZIONE: TRTCT96R1 non riesce a-
027200041207     C                              decod. il Cod.Partner da Tab.(PT). Il mitte-
027300041207     C                             nte ricevuto nell''IFCSUM è >'
027400100812     C                   EVAL      msgDSP  = %Trim(msgDSP) + PT_Parziale +
027500100628     C                             %Trim('< Controllare il Messaggio Ricevuto>')
027600040310      *
027700040310      * INVIA MESSAGGIO ad un Utente --> EDPAB
027800040322     c                   if        User_msg <> *blank
027900040310     c                   z-add     1             Jx
028000040310     C                   exsr      CalQEZSNDMG
028100040322     c                   end
028200040310      *
028300040310     c                   endsr
028400040310     ***********************************************************************
028500040310     **
028600040310     ** Send Message (QEZSNDMG) API
028700040310     **
028800040310     ***********************************************************************
028900040310     C     CalQEZSNDMG   BEGSR
029000040310      *
029100040310     ** Invio messaggio all'utente.
029200040310     C                   EVAL      SndMg03 = msgDSP
029300040322     C******             EVAL      SndMg05(Jx) = 'EDPAB     '
029400040322     C                   EVAL      SndMg05(Jx) = User_msg
029500040310     C                   EVAL      SndMg06 = Jx
029600040310     C                   CLEAR                   QUSEC
029700040310     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029800040310
029900040310     C                   CALL      'QEZSNDMG'
030000040310     C                   PARM                    SndMg01
030100040310     C                   PARM                    SndMg02
030200040310     C                   PARM                    SndMg03
030300040310     C                   PARM                    SndMg04
030400040310     C                   PARM                    SndMg05
030500040310     C                   PARM                    SndMg06
030600040310     C                   PARM                    SndMg07
030700040310     C                   PARM                    SndMg08
030800040310     C                   PARM                    Qusec
030900040310     C                   PARM                    SndMg10
031000040310     C                   PARM                    SndMg11
031100040310     C                   PARM                    SndMg12
031200040310      *
031300040310     C                   ENDSR
031400040310     O*****************************************************************
