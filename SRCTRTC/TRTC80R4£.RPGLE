000100980618      ***********************************************************************
000200070219      *   Chiamato da TRTC80R                        - E.D.I.   ESTERO -    *
000300960329      ***********************************************************************
000400040301 :    * Serve x trascodificare la tabella PT tramite l'UNB di codici dubbi
000500040301      ***********************************************************************
000600070108 :    * es.: PARTNER con più Linee e sulla tabella PT viene codificato
000700070108 :    * più volte esempio: " AZKAR ...... ES " e " AZKAR ...... PT "
000800070108 :    * poichè gestisce 2 nazioni Spagna e Portogallo
000900000126      ***********************************************************************
001000040301 :    * Il programma serve per reperire la Nazione del primo dettaglio mittente
001100070219 :    * dallo stesso ciclo di lettura del Flat file EDIFCSUM senza farlo nel
001200070219 :    * TRTC80R direttamente. Ciò sposizionerebbe il ciclo nel TRTC80R...così no.
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
002400070219     D EDTD15DS      E DS
002500000120      * DS per reperimento destinatario comunicazione
002600960524     D WTD15           DS
002700960524     D  WTCD                   1      3
002800960524     D  WCMD                   4     28
002900040302      * Tabella partner
003000040302     D CPT             S             35    DIM(100)
003100040302     D W035A           s             35
003200000120      *---------------------------------------------------------------------*
003300000120      *                    .. 5 - 8  : tipo record letto
003400960524     D WDAT            DS           587
003500960524     D  SUMTPR                 5      8
003600040310     C*---------------------------------------------------------------*
003700040310     D MsgDSP          S            256                                         Testo generico
003800040310     C*---------------------------------------------------------------*
003900040310     D SndMg01         S             10                                         Message type
004000040310     D                                     INZ('*INFO')
004100040310     D SndMg02         S             10                                         Deluvery mode
004200040310     D                                     INZ('*BREAK')
004300040310     D SndMg03         S            256                                         Message text
004400040310     D SndMg04         S             10I 0                                      Length of text
004500040310     D                                     INZ(%SIZE(SndMg03))
004600040310     D SndMg05         S             10                                         User profile
004700040310     D                                     DIM(299)
004800040310     D SndMg06         S             10I 0                                      Number of user
004900040310     D SndMg07         S             10I 0                                      Message sent indic.
005000040310     D SndMg08         S             10I 0                                      Function requested
005100040310     D SndMg10         S              1                                         Show display
005200040310     D                                     INZ('N')
005300040310     D SndMg11         S             20                                         Qualified MSGQ name
005400040310     D SndMg12         S              4                                         Name type indicator
005500040310     D                                     INZ('*USR')
005600040310      * indice di scihera
005700040310     D Jx              S              5I 0
005800040310      *
005900040310     D/COPY QSYSINC/QRPGLESRC,QUSEC
006000000120      *---------------------------------------------------------------------*
006100960711     IRRIFCSUM
006200960524     I              RRIFCSUM                    SUMDAT
006300000120      *---------------------------------------------------------------------*
006400000120      * Ciclo principale
006500000120      *---------------------------------------------------------------------*
006600040301      *  Parametrti ricevuti:
006700040301      *     Nrrec_a   = Num.Rel.Rec. della testata da cui partire (potebbero esserci
006800040301      *                 più testate/dettagli all'interno dello stesso Flat File
006900040301      *     Quale_Det = in quale dettaglio CZ reperire la nazione del mittente.
007000040301      *                 Solitamente lo si prende dal primo ma se ci fosse qualche
007100040301      *                 problema lo deve cercare sui successivi prendendo il primo
007200040301      *                 valido.
007300070104      *     Naz-Mitt  = è il parametro da restituire al TRTC79R per permettere di
007400040301      *                 trovare l'UNB corretto.
007500040302      *---------------------------------------------------------------------*
007600040301     c     *entry        plist
007700040301     c                   parm                    Nrec              9 0
007800040301     c                   parm                    Naz_mitt          3
007900040302     c                   parm                    Ok_Naz            1
008000040322     c                   parm                    User_msg         10
008100070213     c                   parm                    UNB_Parz         35
008200040302      *
008300040302      * Caricamento Tabella Partner esteri
008400040302     C                   Z-ADD     0             X                 3 0
008500040302     C                   MOVEL     'PT'          TABCOD
008600040302     C     TABCOD        CHAIN     EDTAB01L                           30
008700040302     C     *IN30         DOWEQ     '0'
008800040310     C     TABFLG        ifEQ      *BLANKS
008900040302     C                   ADD       1             X
009000040302     C                   MOVEL     TABKEY        CPT(X)
009100040302     C                   End
009200040302     C     TABCOD        READE     EDTAB01L                               30
009300040302     C                   END
009400040301     C*
009500040301     C* IMPOSTO A ZERO VARIABILI DI PGM
009600040301     C                   MOVEL     'N'           WFINE             1
009700040302     C                   MOVEL     'N'           OK_Naz            1
009800040301     C                   z-add     0             conta             3 0
009900070214     C                   z-add     0             contaTD00         3 0
010000040301      *
010100040302      *  Si posiziona legge e memorizza il Nome del Partner Mittente
010200040302     c                   exsr      X_primacosa
010300980609      *
010400960329      * Lettura fino a fine file
010500040302     C     Wfine         DOWNE     'S'                                          - Fine file -
010600070212      *
010700070212      * ?------------------- *
010800070214      * Se ha finito i dettaglio e legge una successiva testata senza avere
010900070214      *  trovato nulla deve uscire forzatamente
011000070214     C     SUMTPR        ifEQ      'TT00'
011100070214     c     conta         andGT     1
011200070214     C                   leave
011300070214     C                   endif
011400070214      *
011500070214      * se è al dettaglio successivo deve comunque uscire
011600070214     C     SUMTPR        ifeq      'TD00'
011700070214     C     contaTD00     ifgt      0
011800070214     c                   leave
011900070214     c                   else
012000070214     c                   add       1             contaTD00
012100070214     c                   end
012200070214     c                   end
012300980609      *
012400040301     C     SUMTPR        ifeq      'TD15'
012500040302      *               *--------------------*
012600980609     C                   EXSR      DRED15
012700040302      *               *--------------------*
012800040301      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
012900040302     c                   if        OK_Naz = 'S'
013000040301     C                   leave
013100040301     C                   endif
013200040301     C                   endif
013300980609      *
013400960329      * Lettura successiva
013500960329     C                   EXSR      REDFIL
013600960329     C                   END                                                    - Fine file -
013700980609      *
013800040301     c     FINE          tag
013900040310      *
014000040310      * Msg di allerta Traduzione da Flat file non finita bene
014100040310     c                   IF        Ok_Naz = 'N'
014200070213     c*******            exsr      SEND_MSG_EDP
014300040310     C                   END                                                    - Fine file -
014400040310      *
014500960329     C                   SETON                                        LR
014600960524     C*----------------------------------------------------------------
014700040302     C*? x Prima cosa: Si posiziona Legge e memorizza il nome generico del Partner
014800960524     C*----------------------------------------------------------------
014900040302     C     x_Primacosa   BEGSR
015000960524     C*
015100040302      * Posizionamento iniziale sul file
015200040302     C                   EXSR      SETFIL
015300040302      * Lettura sequenziale
015400040302     C                   EXSR      REDFIL
015500040302     C*
015600040302      * Imposta il codice Partner
015700070213     C                   MOVEL     UNB_Parz      WCODPT           35
015800070214     C                   MOVE      '    '        WCODPT
015900040302     C*
016000960524     C                   ENDSR
016100040302     C*----------------------------------------------------------------
016200040302     C*? SETFIL - POSIZIONAMENTO FRA più TESTATE
016300040302     C*----------------------------------------------------------------
016400040302     C     SETFIL        BEGSR
016500040302     C* Lettura file
016600040302     C     NREC          SETLL     EDIFCSUM
016700040302     C*
016800040302     C                   ENDSR
016900040301     C*----------------------------------------------------------------
017000040301     C*? REDFIL - LETTURA FILE
017100040301     C*----------------------------------------------------------------
017200040301     C     REDFIL        BEGSR
017300040301     C* Lettura file
017400040301     C                   READ      EDIFCSUM                               30
017500040301     C   30              MOVEL     'S'           WFINE
017600040301     C  N30              MOVEL     SUMDAT        WDAT
017700040301     C*
017800040301     C                   ENDSR
017900010502     C*----------------------------------------------------------------
018000960527     C*? DRED15 - DECODIFICA TIPO RECORD TD15
018100960527     C*----------------------------------------------------------------
018200960527     C     DRED15        BEGSR
018300000120      *
018400070219     C                   MOVEL     SUMDAT        EDTD15DS
018500980930      * Dati mittente
018600070219     C                   IF         DD3035 = 'CZ' and
018700070219     C                              DD3207 <> *blanks
018800040301     C                   add       1             conta
018900040302      *
019000040302      * Verifica esistenza su tab.:PT
019100070219     C                   eval      W035A = %subst(WCODPT:1:31) + DD3207
019200040302     C                   z-add     1             X
019300040302     C     W035a         lookup    CPT(X)                                 32
019400040302     C                   If        *in32 = *on
019500070219     c                   movel     DD3207        Naz_mitt
019600040302     C                   movel     'S'           OK_Naz
019700070214     c                   movel(p)  W035a         UNB_Parz
019800040302     c                   endIF
019900040302      *
020000000125     C                   ENDIF
020100031211      *
020200960527     C                   ENDSR
020300040310     c*==================================================================*
020400040310      * Manda un Msg a EDP*
020500040310     c*==================================================================*
020600040310     c     SEND_MSG_EDP  begsr
020700040310      *
020800040310      * Imposta la Frase del messaggio
020900040310     c                   clear                   msgDSP
021000070104     C                   EVAL      msgDSP  = 'ATTENZIONE:  TRTC79R non riesce a-
021100041207     C                              decod. il Cod.Partner da Tab.(PT). Il mitte-
021200041207     C                             nte ricevuto nell''IFCSUM è >'
021300041207     C                   EVAL      msgDSP  = %Trim(msgDSP) + WCODPT +
021400041207     C                             %Trim('< Controllare membri su TRIFCSUM o in-
021500041207     C                              RCVIFCSUM records con stato (ER)')
021600040310      *
021700040310      * INVIA MESSAGGIO ad un Utente --> EDPAB
021800040322     c                   if        User_msg <> *blank
021900040310     c                   z-add     1             Jx
022000040310     C                   exsr      CalQEZSNDMG
022100040322     c                   end
022200040310      *
022300040310     c                   endsr
022400040310     ***********************************************************************
022500040310     **
022600040310     ** Send Message (QEZSNDMG) API
022700040310     **
022800040310     ***********************************************************************
022900040310     C     CalQEZSNDMG   BEGSR
023000040310      *
023100040310     ** Invio messaggio all'utente.
023200040310     C                   EVAL      SndMg03 = msgDSP
023300040322     C******             EVAL      SndMg05(Jx) = 'EDPAB     '
023400040322     C                   EVAL      SndMg05(Jx) = User_msg
023500040310     C                   EVAL      SndMg06 = Jx
023600040310     C                   CLEAR                   QUSEC
023700040310     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
023800040310
023900040310     C                   CALL      'QEZSNDMG'
024000040310     C                   PARM                    SndMg01
024100040310     C                   PARM                    SndMg02
024200040310     C                   PARM                    SndMg03
024300040310     C                   PARM                    SndMg04
024400040310     C                   PARM                    SndMg05
024500040310     C                   PARM                    SndMg06
024600040310     C                   PARM                    SndMg07
024700040310     C                   PARM                    SndMg08
024800040310     C                   PARM                    Qusec
024900040310     C                   PARM                    SndMg10
025000040310     C                   PARM                    SndMg11
025100040310     C                   PARM                    SndMg12
025200040310      *
025300040310     C                   ENDSR
025400040310     O*****************************************************************
