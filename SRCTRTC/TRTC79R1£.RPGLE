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
003200040302     D CPT             S             35    DIM(100)
003300040302     D W035A           s             35
003400000120      *---------------------------------------------------------------------*
003500000120      *                    .. 5 - 8  : tipo record letto
003600960524     D WDAT            DS           587
003700960524     D  SUMTPR                 5      8
003800040310     C*---------------------------------------------------------------*
003900040310     D MsgDSP          S            256                                         Testo generico
004000040310     C*---------------------------------------------------------------*
004100040310     D SndMg01         S             10                                         Message type
004200040310     D                                     INZ('*INFO')
004300040310     D SndMg02         S             10                                         Deluvery mode
004400040310     D                                     INZ('*BREAK')
004500040310     D SndMg03         S            256                                         Message text
004600040310     D SndMg04         S             10I 0                                      Length of text
004700040310     D                                     INZ(%SIZE(SndMg03))
004800040310     D SndMg05         S             10                                         User profile
004900040310     D                                     DIM(299)
005000040310     D SndMg06         S             10I 0                                      Number of user
005100040310     D SndMg07         S             10I 0                                      Message sent indic.
005200040310     D SndMg08         S             10I 0                                      Function requested
005300040310     D SndMg10         S              1                                         Show display
005400040310     D                                     INZ('N')
005500040310     D SndMg11         S             20                                         Qualified MSGQ name
005600040310     D SndMg12         S              4                                         Name type indicator
005700040310     D                                     INZ('*USR')
005800040310      * indice di scihera
005900040310     D Jx              S              5I 0
006000040310      *
006100040310     D/COPY QSYSINC/QRPGLESRC,QUSEC
006200000120      *---------------------------------------------------------------------*
006300960711     IRRIFCSUM
006400960524     I              RRIFCSUM                    SUMDAT
006500000120      *---------------------------------------------------------------------*
006600000120      * Ciclo principale
006700000120      *---------------------------------------------------------------------*
006800040301      *  Parametrti ricevuti:
006900040301      *     Nrrec_a   = Num.Rel.Rec. della testata da cui partire (potebbero esserci
007000040301      *                 più testate/dettagli all'interno dello stesso Flat File
007100040301      *     Quale_Det = in quale dettaglio CZ reperire la nazione del mittente.
007200040301      *                 Solitamente lo si prende dal primo ma se ci fosse qualche
007300040301      *                 problema lo deve cercare sui successivi prendendo il primo
007400040301      *                 valido.
007500070104      *     Naz-Mitt  = è il parametro da restituire al TRTC79R per permettere di
007600040301      *                 trovare l'UNB corretto.
007700040301      *---------------------------------------------------------------------*
007800070108     C     *LIKE         DEFINE    T000004       WCODPT
007900040302      *---------------------------------------------------------------------*
008000040301     c     *entry        plist
008100040301     c                   parm                    Nrec              9 0
008200040301     c                   parm                    Naz_mitt          3
008300040302     c                   parm                    Ok_Naz            1
008400040322     c                   parm                    User_msg         10
008500040302      *
008600040302      * Caricamento Tabella Partner esteri
008700040302     C                   Z-ADD     0             X                 3 0
008800040302     C                   MOVEL     'PT'          TABCOD
008900040302     C     TABCOD        CHAIN     EDTAB01L                           30
009000040302     C     *IN30         DOWEQ     '0'
009100040310     C     TABFLG        ifEQ      *BLANKS
009200040302     C                   ADD       1             X
009300040302     C                   MOVEL     TABKEY        CPT(X)
009400040302     C                   End
009500040302     C     TABCOD        READE     EDTAB01L                               30
009600040302     C                   END
009700040301     C*
009800040301     C* IMPOSTO A ZERO VARIABILI DI PGM
009900040301     C                   MOVEL     'N'           WFINE             1
010000040302     C                   MOVEL     'N'           OK_Naz            1
010100040301     C                   z-add     0             conta             3 0
010200040301      *
010300040302      *  Si posiziona legge e memorizza il Nome del Partner Mittente
010400040302     c                   exsr      X_primacosa
010500980609      *
010600960329      * Lettura fino a fine file
010700040302     C     Wfine         DOWNE     'S'                                          - Fine file -
010800070212      *
010900070212      * ?------------------- *
011000070212      * ?Prova prima se c'è in testata il mittente x tutti poi in dettaglio
011100070212     C     SUMTPR        ifeq      'TT25'
011200070212      *               *--------------------*
011300070212     C                   EXSR      DRET25
011400070212      *               *--------------------*
011500070212      * ?deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
011600070212     c                   if        OK_Naz = 'S'
011700070212     C                   leave
011800070212     C                   endif
011900070212     C                   endif
012000070212      * ?------------------- *
012100980609      *
012200040301     C     SUMTPR        ifeq      'TD15'
012300040302      *               *--------------------*
012400980609     C                   EXSR      DRED15
012500040302      *               *--------------------*
012600040301      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
012700040302     c                   if        OK_Naz = 'S'
012800040301     C                   leave
012900040301     C                   endif
013000040301     C                   endif
013100980609      *
013200040301      * Se ha finito i dettaglio e legge una successiva testata senza avere
013300040301      *  trovato nulla deve uscire forzatamente
013400040302     C     SUMTPR        ifEQ      'TT00'
013500040302     c     conta         andGT     1
013600040301     C                   leave
013700040301     C                   endif
013800960329      * Lettura successiva
013900960329     C                   EXSR      REDFIL
014000960329     C                   END                                                    - Fine file -
014100980609      *
014200040301     c     FINE          tag
014300040310      *
014400040310      * Msg di allerta Traduzione da Flat file non finita bene
014500040310     c                   IF        Ok_Naz = 'N'
014600040310     c                   exsr      SEND_MSG_EDP
014700040310     C                   END                                                    - Fine file -
014800040310      *
014900960329     C                   SETON                                        LR
015000960524     C*----------------------------------------------------------------
015100040302     C*? x Prima cosa: Si posiziona Legge e memorizza il nome generico del Partner
015200960524     C*----------------------------------------------------------------
015300040302     C     x_Primacosa   BEGSR
015400960524     C*
015500040302      * Posizionamento iniziale sul file
015600040302     C                   EXSR      SETFIL
015700040302      * Lettura sequenziale
015800040302     C                   EXSR      REDFIL
015900040302     C*
016000040302      * Imposta il codice Partner
016100040302     C     SUMTPR        ifeq      'TT00'
016200040302     C*
016300070108     C                   MOVEL     SUMDAT        ED6T00DS
016400070108     C                   MOVEL     T000004       WCODPT
016500040302     C*
016600040302     C                   else
016700040302     C*
016800040302     C* Se il primo record letto non è la testata Partner UNB
016900040302     C*  deve uscire dal pgm senza dare nessun esito
017000040302     c                   move      'S'           Wfine
017100040302     C*
017200040302     C                   endif
017300040302     C*
017400960524     C                   ENDSR
017500040302     C*----------------------------------------------------------------
017600040302     C*? SETFIL - POSIZIONAMENTO FRA più TESTATE
017700040302     C*----------------------------------------------------------------
017800040302     C     SETFIL        BEGSR
017900040302     C* Lettura file
018000040302     C     NREC          SETLL     EDIFCSUM
018100040302     C*
018200040302     C                   ENDSR
018300040301     C*----------------------------------------------------------------
018400040301     C*? REDFIL - LETTURA FILE
018500040301     C*----------------------------------------------------------------
018600040301     C     REDFIL        BEGSR
018700040301     C* Lettura file
018800040301     C                   READ      EDIFCSUM                               30
018900040301     C   30              MOVEL     'S'           WFINE
019000040301     C  N30              MOVEL     SUMDAT        WDAT
019100040301     C*
019200040301     C                   ENDSR
019300070212     C*----------------------------------------------------------------
019400070212     C*? DRET25 - DECODIFICA TIPO RECORD TT25
019500070212     C*----------------------------------------------------------------
019600070212     C     DRET25        BEGSR
019700070212      *
019800070212     C                   MOVEL     SUMDAT        ED6T25DS
019900070212      * ?Dati mittente  ---- *
020000070212     C                   IF         T253035 = 'CZ' and
020100070212     C                              T253207 <> *blanks
020200070212     C                   add       1             conta
020300070212      *
020400070212      * ?Verifica esistenza su tab.:PT
020500070212     C                   eval      W035A = %subst(WCODPT:1:31) + T253207
020600070212     C                   z-add     1             X
020700070212     C     W035a         lookup    CPT(X)                                 32
020800070212     C                   If        *in32 = *on
020900070212     c                   movel     T253207       Naz_mitt
021000070212     C                   movel     'S'           OK_Naz
021100070212     c                   endIF
021200070212      *
021300070212     C                   ENDIF
021400070212      *
021500070212     C                   ENDSR
021600010502     C*----------------------------------------------------------------
021700960527     C*? DRED15 - DECODIFICA TIPO RECORD TD15
021800960527     C*----------------------------------------------------------------
021900960527     C     DRED15        BEGSR
022000000120      *
022100070108     C                   MOVEL     SUMDAT        ED6D15DS
022200980930      * Dati mittente
022300070108     C                   IF         D153035 = 'CZ' and
022400070108     C                              D153207 <> *blanks
022500040301     C                   add       1             conta
022600040302      *
022700040302      * Verifica esistenza su tab.:PT
022800070108     C                   eval      W035A = %subst(WCODPT:1:31) + D153207
022900040302     C                   z-add     1             X
023000040302     C     W035a         lookup    CPT(X)                                 32
023100040302     C                   If        *in32 = *on
023200070108     c                   movel     D153207       Naz_mitt
023300040302     C                   movel     'S'           OK_Naz
023400040302     c                   endIF
023500040302      *
023600000125     C                   ENDIF
023700031211      *
023800960527     C                   ENDSR
023900040310     c*==================================================================*
024000040310      * Manda un Msg a EDP*
024100040310     c*==================================================================*
024200040310     c     SEND_MSG_EDP  begsr
024300040310      *
024400040310      * Imposta la Frase del messaggio
024500040310     c                   clear                   msgDSP
024600070104     C                   EVAL      msgDSP  = 'ATTENZIONE:  TRTC79R non riesce a-
024700041207     C                              decod. il Cod.Partner da Tab.(PT). Il mitte-
024800041207     C                             nte ricevuto nell''IFCSUM è >'
024900041207     C                   EVAL      msgDSP  = %Trim(msgDSP) + WCODPT +
025000041207     C                             %Trim('< Controllare membri su TRIFCSUM o in-
025100041207     C                              RCVIFCSUM records con stato (ER)')
025200040310      *
025300040310      * INVIA MESSAGGIO ad un Utente --> EDPAB
025400040322     c                   if        User_msg <> *blank
025500040310     c                   z-add     1             Jx
025600040310     C                   exsr      CalQEZSNDMG
025700040322     c                   end
025800040310      *
025900040310     c                   endsr
026000040310     ***********************************************************************
026100040310     **
026200040310     ** Send Message (QEZSNDMG) API
026300040310     **
026400040310     ***********************************************************************
026500040310     C     CalQEZSNDMG   BEGSR
026600040310      *
026700040310     ** Invio messaggio all'utente.
026800040310     C                   EVAL      SndMg03 = msgDSP
026900040322     C******             EVAL      SndMg05(Jx) = 'EDPAB     '
027000040322     C                   EVAL      SndMg05(Jx) = User_msg
027100040310     C                   EVAL      SndMg06 = Jx
027200040310     C                   CLEAR                   QUSEC
027300040310     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
027400040310
027500040310     C                   CALL      'QEZSNDMG'
027600040310     C                   PARM                    SndMg01
027700040310     C                   PARM                    SndMg02
027800040310     C                   PARM                    SndMg03
027900040310     C                   PARM                    SndMg04
028000040310     C                   PARM                    SndMg05
028100040310     C                   PARM                    SndMg06
028200040310     C                   PARM                    SndMg07
028300040310     C                   PARM                    SndMg08
028400040310     C                   PARM                    Qusec
028500040310     C                   PARM                    SndMg10
028600040310     C                   PARM                    SndMg11
028700040310     C                   PARM                    SndMg12
028800040310      *
028900040310     C                   ENDSR
029000040310     O*****************************************************************
