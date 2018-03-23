000100980618      ***********************************************************************
000200130411      *   Chiamato da TRTCT98R                       - E.D.I.   ESTERO -    *
000300960329      ***********************************************************************
000400040301 :    * Serve x trascodificare la tabella PT tramite l'UNB di codici dubbi
000500100628 :    * che gestiscono 2 nazioni
000600040301      ***********************************************************************
000700100628 :    * in questo modo posso prendere o dal primo dettaglio se soddisfa
000800100628 :    * l'aggancio dell'UNB oppure dal primo dettaglio valido successivo.
000900130411 :    * (Il NAD+HI opportuno per poter decodificare la tabella PT.)
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
003700100628???? D edS00UNB      E DS
003800100628???? D edS00NAD      E DS
003900100628      *---------------------------------------------------------------------*
004000100628     D Valori_inErr    ds
004100100628     D  SKout_Errori                  1    DIM(50)
004200100628     D Descr_Errore    ds
004300100628     D  SKout_DesErr                 50    DIM(50)
004400100628      *
004500100628     C*---------------------------------------------------------------*
004600100628      * Tabella partner
004700100628     C*---------------------------------------------------------------*
004800130429     D EDIDSPT       E DS
004900130429     D CPT             S             35    DIM(200)
005000100628     D  W035A          s             35
005100100628     D  almeno_uno     s              1
005200040310     C*---------------------------------------------------------------*
005300040310     D MsgDSP          S            256                                         Testo generico
005400040310     C*---------------------------------------------------------------*
005500040310     D SndMg01         S             10                                         Message type
005600040310     D                                     INZ('*INFO')
005700040310     D SndMg02         S             10                                         Deluvery mode
005800040310     D                                     INZ('*BREAK')
005900040310     D SndMg03         S            256                                         Message text
006000040310     D SndMg04         S             10I 0                                      Length of text
006100040310     D                                     INZ(%SIZE(SndMg03))
006200040310     D SndMg05         S             10                                         User profile
006300040310     D                                     DIM(299)
006400040310     D SndMg06         S             10I 0                                      Number of user
006500040310     D SndMg07         S             10I 0                                      Message sent indic.
006600040310     D SndMg08         S             10I 0                                      Function requested
006700040310     D SndMg10         S              1                                         Show display
006800040310     D                                     INZ('N')
006900040310     D SndMg11         S             20                                         Qualified MSGQ name
007000040310     D SndMg12         S              4                                         Name type indicator
007100040310     D                                     INZ('*USR')
007200040310      * indice di scihera
007300040310     D Jx              S              5I 0
007400040310      *
007500040310     D/COPY QSYSINC/QRPGLESRC,QUSEC
007600100628      *---------------------------------------------------------------------*
007700130411     d Legge_UNB_NADHI...
007800100628     d                 C                   CONST('SELECT RRN(tivin00r), vindta -
007900100707     d                                         FROM tivin00r WHERE VINFLG='' ''-
008000100707     d                                          and ( VINDTA like ''UNB+%'' or -
008100130411     d                                         VINDTA like ''NAD+HI%'')  -
008200100707     d                                          and RRN(tivin00r)>= ')
008300100707     d Ordina_SQL      c                   CONST(' ORDER BY RRN(tivin00r)')
008400100628     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008500100628     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008600000120      *---------------------------------------------------------------------*
008700000120      * Ciclo principale
008800000120      *---------------------------------------------------------------------*
008900040301      *  Parametrti ricevuti:
009000100628      *     Nrrec_a   = Num.Rel.Rec. Non serve più in questo programma
009100130411      *     Quale_Det = in quale dettaglio HI reperire la nazione del mittente.
009200040301      *                 Solitamente lo si prende dal primo ma se ci fosse qualche
009300040301      *                 problema lo deve cercare sui successivi prendendo il primo
009400040301      *                 valido.
009500130411      *     Naz-Mitt  = è il parametro da restituire al TRTCT98R per permettere di
009600040301      *                 trovare l'UNB corretto.
009700040301      *---------------------------------------------------------------------*
009800040301     c     *entry        plist
009900040301     c                   parm                    Nrec              9 0
010000100813     c                   parm                    PT_Parziale      35
010100040301     c                   parm                    Naz_mitt          3
010200040302     c                   parm                    Ok_Naz            1
010300040322     c                   parm                    User_msg         10
010400100716     C                   parm                    keyUNBCLI
010500100716     C                   parm                    keyTIPOMSG
010600100716     C                   parm                    keyVERSION
010700100716     C                   parm                    keyRELEASE
010800100716     C                   parm                    keyAGENCY
010900100716     C                   parm                    keyASSOCIA
011000040302      *
011100040302      * Caricamento Tabella Partner esteri
011200040302     C                   Z-ADD     0             X                 3 0
011300040302     C                   MOVEL     'PT'          TABCOD
011400040302     C     TABCOD        CHAIN     EDTAB01L                           30
011500040302     C     *IN30         DOWEQ     '0'
011600040310     C     TABFLG        ifEQ      *BLANKS
011700040302     C                   ADD       1             X
011800040302     C                   MOVEL     TABKEY        CPT(X)
011900040302     C                   End
012000040302     C     TABCOD        READE     EDTAB01L                               30
012100040302     C                   END
012200040301     C*
012300040301     C* IMPOSTO A ZERO VARIABILI DI PGM
012400040301     C                   MOVEL     'N'           WFINE             1
012500040302     C                   MOVEL     'N'           OK_Naz            1
012600130411     C                   z-add     0             conta_NAD_HI      5 0
012700100628     c                   z-add     0             NRelRec_VIN       9 0
012800040301      *
012900130411     c                   eval      stringa_SQL = Legge_UNB_NADHI
013000100707     c                   eval      stringa_SQL = %trim(stringa_SQL) +
013100100707     c                             %trim(%editc(Nrec:'Z')) +
013200100707     c                             ordina_sql
013300100628      *
013400130411      * Lettura sequenziale solo dei segmenti (UNB) e (NAD+HI)
013500100628     C/EXEC SQL
013600100628     C+ PREPARE Legge_TIVIN00R FROM :stringa_SQL
013700100628     C/END-EXEC
013800100628
013900100628     C/EXEC SQL
014000100628     C+ DECLARE Cursore_TIVIN CURSOR FOR Legge_TIVIN00R
014100100628     C/END-EXEC
014200100628
014300100628     C/EXEC SQL
014400100628     C+ OPEN Cursore_TIVIN
014500100628     C/END-EXEC
014600100628     c                   clear                   almeno_uno
014700100628
014800100628     C                   DOU       SqlCod <> 0
014900100628      *
015000130411      * Lettura sequenziale solo dei segmenti (UNB) e (NAD+HI)
015100100628     C/EXEC SQL
015200100628     C+ FETCH NEXT FROM Cursore_TIVIN INTO :NRelRec_VIN, :VINDTA
015300100628     C/END-EXEC
015400100628     C                   SELECT
015500100628     c* a fine file
015600100628     C                   WHEN      SqlCod = 100  or Wfine = 'S'
015700100628     c                   leave
015800100628     **
015900100628     C                   WHEN      SqlCod < 0
016000100628     C                   seton                                        H2
016100100628     c                   leave
016200100628     c*
016300100628     C                   OTHER
016400100628     c* dettaglio
016500100628     c                   move      'S'           almeno_uno
016600100628      *
016700100628      ** Decodifica record a campi variabili
016800100628      * ?              /*---------------------- */
016900100628     c                   exsr      Decod_Segmento
017000100628      * ?              /*---------------------- */
017100100628      *
017200100628      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
017300100628     c                   if        OK_Naz = 'S'
017400100628     C                   leave
017500100628     C                   endif
017600100628      *
017700100628      * Se ha finito i dettaglio e legge una successiva testata senza avere
017800100628      *  trovato nulla deve uscire forzatamente
017900130411     c                   if        tipo_seg = 'UNB' and conta_NAD_HI >= 1
018000100628     C                   leave
018100100628     C                   endif
018200100628      **
018300100628     C                   ENDSL
018400100628      **
018500100628     C                   ENDDO
018600100628     C/EXEC SQL
018700100628     C+ CLOSE Cursore_TIVIN
018800100628     C/END-EXEC
018900040310      *
019000040310      * Msg di allerta Traduzione da Flat file non finita bene
019100040310     c                   IF        Ok_Naz = 'N'
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
020900100720     C                   clear                   keyUNBCLI
021000100720     C                   clear                   keyTIPOMSG
021100100720     C                   clear                   keyVERSION
021200100720     C                   clear                   keyRELEASE
021300100720     C                   clear                   keyAGENCY
021400100720     C                   clear                   keyASSOCIA
021500100720      *
021600100720      *  Chiamata generica x decodificare il singolo segmento letto
021700100720     c                   call      'TRTCT00R1'
021800100720      * input
021900100720     c                   parm                    tipo_seg
022000100720     c                   parm                    segmento
022100100720     C                   parm                    keyUNBCLI
022200100720     C                   parm                    keyTIPOMSG
022300100720     C                   parm                    keyVERSION
022400100720     C                   parm                    keyRELEASE
022500100720     C                   parm                    keyAGENCY
022600100720     C                   parm                    keyASSOCIA
022700100720      *output
022800100720     c                   parm                    Errore
022900100720     c                   parm                    DsGenerica
023000100720     c                   parm                    Valori_inErr
023100100720     c                   parm                    Descr_Errore
023200100720     c                   parm                    trovato_seg
023300100720      **--------
023400100720      *  Ritorna la DS GENERICA oppure l'errore
023500100628      **--------
023600100628      *  Contatore delle righe ossia dei segmenti del messaggio
023700100628     c                   add       1             Conta_segmenti    5 0
023800100628      **--------
023900100628     c                   select
024000100628      **--------
024100100628     c                   when      tipo_seg = 'UNB'
024200100720     c                   movel(p)  DsGenerica    EDS00UNB
024300100628      *
024400100628      **--------
024500100628     c                   when      tipo_seg = 'NAD'
024600100720     c                   movel(p)  DsGenerica    EDS00NAD
024700100628     C*
024800100628     C* Se il primo record letto non è la testata Partner UNB
024900100628     C*  deve uscire dal pgm senza dare nessun esito
025000100813     c                   If          PT_Parziale = *blank
025100100628     c                   move      'S'           Wfine                          --> Esce in Errore
025200100628     c                   ELSE
025300100628      *
025400100628      * Dati mittente  con la NAZIONE mittente:
025500100628     C                   if         NAD3207 <> *blanks
025600130411     C                   add       1             conta_NAD_HI
025700100628      * Verifica esistenza su tab.:PT
025800100813     C                   eval      W035A = %subst(PT_Parziale:1:31) + NAD3207
025900100628     C                   z-add     1             X
026000100628     C     W035a         lookup    CPT(X)                                 32
026100100628     C                   If        *in32 = *on
026200100628     c                   movel     NAD3207       Naz_mitt                       --> Decodificata
026300100628     C                   movel     'S'           OK_Naz                             La NAZIONE
026400100628     c                   z-add     NRelRec_VIN   Nrec
026500100628     c                   endIF
026600100628     c                   end
026700100628      *
026800100628     C                   ENDIF
026900100628      **--------
027000100628     c                   endSL
027100100628      **
027200100628     c     end_Decod     endSR
027300040310     c*==================================================================*
