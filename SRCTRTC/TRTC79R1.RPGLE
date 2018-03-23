000100980618      ***********************************************************************
000200070104      *   Chiamato da TRTC79R                        - E.D.I.   ESTERO -    *
000300960329      ***********************************************************************
000400040301 :    * Serve x trascodificare la tabella PT tramite l'UNB di codici dubbi
000500040301      ***********************************************************************
000600070108 :    * es.: PARTNER con più Linee e sulla tabella PT viene codificato
000700070108 :    * più volte esempio: " AZKAR ...... ES " e " AZKAR ...... PT "
000800070108 :    * poichè gestisce 2 nazioni Spagna e Portogallo
000900000126      ***********************************************************************
001000040301 :    * Il programma serve per reperire la Nazione del primo dettaglio mittente
001100040301 :    * dallo stesso ciclo di lettura del Flat file TRIFCSUM senza farlo nel
001200070104 :    * TRTC79R direttamente. Ciò sposizionerebbe il ciclo nel TRTC79R...così no.
001300040301      ***********************************************************************
001400040301 :    * in questo modo posso quindi prendere o dal primo dettaglio se soddisfa
001500040301 :    * il riconoscimento dell'UNB oppure dal primo dettaglio valido successivo.
001600040301 :    * il NAD+CZ opportuno per poter decodificare la tabella PT.
001700040301      ***********************************************************************
001800960329     H DECEDIT('0,') DATEDIT(*DMY.)
001900000120      *---------------------------------------------------------------------*
002000040301     FEDIFCSUM  iF   E             DISK
002100040302     FEDTAB01L  IF   E           K DISK
002200000120      *---------------------------------------------------------------------*
002300000120      * .. per scompozione dati ricevuti a seconda del tipo record
002400070108     D ED6T00DS      E DS
002500070212     D ED6T25DS      E DS
002600070212     D ED6D15DS      E DS
002700000120      * DS per reperimento destinatario comunicazione
002800960524     D WTD15           DS
002900960524     D  WTCD                   1      3
003000960524     D  WCMD                   4     28
003100040302      * Tabella partner
003200130429     D EDIDSPT       E DS
003300130429     D CPT             S             35    DIM(200)
003400040302     D W035A           s             35
003500000120      *---------------------------------------------------------------------*
003600000120      *                    .. 5 - 8  : tipo record letto
003700960524     D WDAT            DS           587
003800960524     D  SUMTPR                 5      8
003900040310     C*---------------------------------------------------------------*
004000040310     D MsgDSP          S            256                                         Testo generico
004100040310     C*---------------------------------------------------------------*
004200040310     D SndMg01         S             10                                         Message type
004300040310     D                                     INZ('*INFO')
004400040310     D SndMg02         S             10                                         Deluvery mode
004500040310     D                                     INZ('*BREAK')
004600040310     D SndMg03         S            256                                         Message text
004700040310     D SndMg04         S             10I 0                                      Length of text
004800040310     D                                     INZ(%SIZE(SndMg03))
004900040310     D SndMg05         S             10                                         User profile
005000040310     D                                     DIM(299)
005100040310     D SndMg06         S             10I 0                                      Number of user
005200040310     D SndMg07         S             10I 0                                      Message sent indic.
005300040310     D SndMg08         S             10I 0                                      Function requested
005400040310     D SndMg10         S              1                                         Show display
005500040310     D                                     INZ('N')
005600040310     D SndMg11         S             20                                         Qualified MSGQ name
005700040310     D SndMg12         S              4                                         Name type indicator
005800040310     D                                     INZ('*USR')
005900040310      * indice di scihera
006000040310     D Jx              S              5I 0
006100040310      *
006200040310     D/COPY QSYSINC/QRPGLESRC,QUSEC
006300000120      *---------------------------------------------------------------------*
006400960711     IRRIFCSUM
006500960524     I              RRIFCSUM                    SUMDAT
006600000120      *---------------------------------------------------------------------*
006700000120      * Ciclo principale
006800000120      *---------------------------------------------------------------------*
006900040301      *  Parametrti ricevuti:
007000040301      *     Nrrec_a   = Num.Rel.Rec. della testata da cui partire (potebbero esserci
007100040301      *                 più testate/dettagli all'interno dello stesso Flat File
007200040301      *     Quale_Det = in quale dettaglio CZ reperire la nazione del mittente.
007300040301      *                 Solitamente lo si prende dal primo ma se ci fosse qualche
007400040301      *                 problema lo deve cercare sui successivi prendendo il primo
007500040301      *                 valido.
007600070104      *     Naz-Mitt  = è il parametro da restituire al TRTC79R per permettere di
007700040301      *                 trovare l'UNB corretto.
007800040301      *---------------------------------------------------------------------*
007900070108     C     *LIKE         DEFINE    T000004       WCODPT
008000040302      *---------------------------------------------------------------------*
008100040301     c     *entry        plist
008200040301     c                   parm                    Nrec              9 0
008300040301     c                   parm                    Naz_mitt          3
008400040302     c                   parm                    Ok_Naz            1
008500040322     c                   parm                    User_msg         10
008600040302      *
008700040302      * Caricamento Tabella Partner esteri
008800040302     C                   Z-ADD     0             X                 3 0
008900040302     C                   MOVEL     'PT'          TABCOD
009000040302     C     TABCOD        CHAIN     EDTAB01L                           30
009100040302     C     *IN30         DOWEQ     '0'
009200040310     C     TABFLG        ifEQ      *BLANKS
009300040302     C                   ADD       1             X
009400040302     C                   MOVEL     TABKEY        CPT(X)
009500040302     C                   End
009600040302     C     TABCOD        READE     EDTAB01L                               30
009700040302     C                   END
009800040301     C*
009900040301     C* IMPOSTO A ZERO VARIABILI DI PGM
010000040301     C                   MOVEL     'N'           WFINE             1
010100040302     C                   MOVEL     'N'           OK_Naz            1
010200040301     C                   z-add     0             conta             3 0
010300040301      *
010400040302      *  Si posiziona legge e memorizza il Nome del Partner Mittente
010500040302     c                   exsr      X_primacosa
010600980609      *
010700960329      * Lettura fino a fine file
010800040302     C     Wfine         DOWNE     'S'                                          - Fine file -
010900070212      *
011000070212      * ?------------------- *
011100070212      * ?Prova prima se c'è in testata il mittente x tutti poi in dettaglio
011200070212     C     SUMTPR        ifeq      'TT25'
011300070212      *               *--------------------*
011400070212     C                   EXSR      DRET25
011500070212      *               *--------------------*
011600070212      * ?deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
011700070212     c                   if        OK_Naz = 'S'
011800070212     C                   leave
011900070212     C                   endif
012000070212     C                   endif
012100070212      * ?------------------- *
012200980609      *
012300040301     C     SUMTPR        ifeq      'TD15'
012400040302      *               *--------------------*
012500980609     C                   EXSR      DRED15
012600040302      *               *--------------------*
012700040301      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
012800040302     c                   if        OK_Naz = 'S'
012900040301     C                   leave
013000040301     C                   endif
013100040301     C                   endif
013200980609      *
013300040301      * Se ha finito i dettaglio e legge una successiva testata senza avere
013400040301      *  trovato nulla deve uscire forzatamente
013500040302     C     SUMTPR        ifEQ      'TT00'
013600040302     c     conta         andGT     1
013700040301     C                   leave
013800040301     C                   endif
013900960329      * Lettura successiva
014000960329     C                   EXSR      REDFIL
014100960329     C                   END                                                    - Fine file -
014200980609      *
014300040301     c     FINE          tag
014400040310      *
014500040310      * Msg di allerta Traduzione da Flat file non finita bene
014600040310     c                   IF        Ok_Naz = 'N'
014700040310     c                   exsr      SEND_MSG_EDP
014800040310     C                   END                                                    - Fine file -
014900040310      *
015000960329     C                   SETON                                        LR
015100960524     C*----------------------------------------------------------------
015200040302     C*? x Prima cosa: Si posiziona Legge e memorizza il nome generico del Partner
015300960524     C*----------------------------------------------------------------
015400040302     C     x_Primacosa   BEGSR
015500960524     C*
015600040302      * Posizionamento iniziale sul file
015700040302     C                   EXSR      SETFIL
015800040302      * Lettura sequenziale
015900040302     C                   EXSR      REDFIL
016000040302     C*
016100040302      * Imposta il codice Partner
016200040302     C     SUMTPR        ifeq      'TT00'
016300040302     C*
016400070108     C                   MOVEL     SUMDAT        ED6T00DS
016500070108     C                   MOVEL     T000004       WCODPT
016600040302     C*
016700040302     C                   else
016800040302     C*
016900040302     C* Se il primo record letto non è la testata Partner UNB
017000040302     C*  deve uscire dal pgm senza dare nessun esito
017100040302     c                   move      'S'           Wfine
017200040302     C*
017300040302     C                   endif
017400040302     C*
017500960524     C                   ENDSR
017600040302     C*----------------------------------------------------------------
017700040302     C*? SETFIL - POSIZIONAMENTO FRA più TESTATE
017800040302     C*----------------------------------------------------------------
017900040302     C     SETFIL        BEGSR
018000040302     C* Lettura file
018100040302     C     NREC          SETLL     EDIFCSUM
018200040302     C*
018300040302     C                   ENDSR
018400040301     C*----------------------------------------------------------------
018500040301     C*? REDFIL - LETTURA FILE
018600040301     C*----------------------------------------------------------------
018700040301     C     REDFIL        BEGSR
018800040301     C* Lettura file
018900040301     C                   READ      EDIFCSUM                               30
019000040301     C   30              MOVEL     'S'           WFINE
019100040301     C  N30              MOVEL     SUMDAT        WDAT
019200040301     C*
019300040301     C                   ENDSR
019400070212     C*----------------------------------------------------------------
019500070212     C*? DRET25 - DECODIFICA TIPO RECORD TT25
019600070212     C*----------------------------------------------------------------
019700070212     C     DRET25        BEGSR
019800070212      *
019900070212     C                   MOVEL     SUMDAT        ED6T25DS
020000070212      * ?Dati mittente  ---- *
020100070212     C                   IF         T253035 = 'CZ' and
020200070212     C                              T253207 <> *blanks
020300070212     C                   add       1             conta
020400070212      *
020500070212      * ?Verifica esistenza su tab.:PT
020600070212     C                   eval      W035A = %subst(WCODPT:1:31) + T253207
020700070212     C                   z-add     1             X
020800070212     C     W035a         lookup    CPT(X)                                 32
020900070212     C                   If        *in32 = *on
021000070212     c                   movel     T253207       Naz_mitt
021100070212     C                   movel     'S'           OK_Naz
021200070212     c                   endIF
021300070212      *
021400070212     C                   ENDIF
021500070212      *
021600070212     C                   ENDSR
021700010502     C*----------------------------------------------------------------
021800960527     C*? DRED15 - DECODIFICA TIPO RECORD TD15
021900960527     C*----------------------------------------------------------------
022000960527     C     DRED15        BEGSR
022100000120      *
022200070108     C                   MOVEL     SUMDAT        ED6D15DS
022300980930      * Dati mittente
022400070108     C                   IF         D153035 = 'CZ' and
022500070108     C                              D153207 <> *blanks
022600040301     C                   add       1             conta
022700040302      *
022800040302      * Verifica esistenza su tab.:PT
022900070108     C                   eval      W035A = %subst(WCODPT:1:31) + D153207
023000040302     C                   z-add     1             X
023100040302     C     W035a         lookup    CPT(X)                                 32
023200040302     C                   If        *in32 = *on
023300070108     c                   movel     D153207       Naz_mitt
023400040302     C                   movel     'S'           OK_Naz
023500040302     c                   endIF
023600040302      *
023700000125     C                   ENDIF
023800031211      *
023900960527     C                   ENDSR
024000040310     c*==================================================================*
024100040310      * Manda un Msg a EDP*
024200040310     c*==================================================================*
024300040310     c     SEND_MSG_EDP  begsr
024400040310      *
024500040310      * Imposta la Frase del messaggio
024600040310     c                   clear                   msgDSP
024700070104     C                   EVAL      msgDSP  = 'ATTENZIONE:  TRTC79R non riesce a-
024800041207     C                              decod. il Cod.Partner da Tab.(PT). Il mitte-
024900041207     C                             nte ricevuto nell''IFCSUM è >'
025000041207     C                   EVAL      msgDSP  = %Trim(msgDSP) + WCODPT +
025100041207     C                             %Trim('< Controllare membri su TRIFCSUM o in-
025200041207     C                              RCVIFCSUM records con stato (ER)')
025300040310      *
025400040310      * INVIA MESSAGGIO ad un Utente --> EDPAB
025500040322     c                   if        User_msg <> *blank
025600040310     c                   z-add     1             Jx
025700040310     C                   exsr      CalQEZSNDMG
025800040322     c                   end
025900040310      *
026000040310     c                   endsr
026100040310     ***********************************************************************
026200040310     **
026300040310     ** Send Message (QEZSNDMG) API
026400040310     **
026500040310     ***********************************************************************
026600040310     C     CalQEZSNDMG   BEGSR
026700040310      *
026800040310     ** Invio messaggio all'utente.
026900040310     C                   EVAL      SndMg03 = msgDSP
027000040322     C******             EVAL      SndMg05(Jx) = 'EDPAB     '
027100040322     C                   EVAL      SndMg05(Jx) = User_msg
027200040310     C                   EVAL      SndMg06 = Jx
027300040310     C                   CLEAR                   QUSEC
027400040310     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
027500040310
027600040310     C                   CALL      'QEZSNDMG'
027700040310     C                   PARM                    SndMg01
027800040310     C                   PARM                    SndMg02
027900040310     C                   PARM                    SndMg03
028000040310     C                   PARM                    SndMg04
028100040310     C                   PARM                    SndMg05
028200040310     C                   PARM                    SndMg06
028300040310     C                   PARM                    SndMg07
028400040310     C                   PARM                    SndMg08
028500040310     C                   PARM                    Qusec
028600040310     C                   PARM                    SndMg10
028700040310     C                   PARM                    SndMg11
028800040310     C                   PARM                    SndMg12
028900040310      *
029000040310     C                   ENDSR
029100040310     O*****************************************************************
