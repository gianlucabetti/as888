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
003000130429     D EDIDSPT       E DS
003100130429     D CPT             S             35    DIM(200)
003200040302     D W035A           s             35
003300000120      *---------------------------------------------------------------------*
003400000120      *                    .. 5 - 8  : tipo record letto
003500960524     D WDAT            DS           587
003600960524     D  SUMTPR                 5      8
003700040310     C*---------------------------------------------------------------*
003800040310     D MsgDSP          S            256                                         Testo generico
003900040310     C*---------------------------------------------------------------*
004000040310     D SndMg01         S             10                                         Message type
004100040310     D                                     INZ('*INFO')
004200040310     D SndMg02         S             10                                         Deluvery mode
004300040310     D                                     INZ('*BREAK')
004400040310     D SndMg03         S            256                                         Message text
004500040310     D SndMg04         S             10I 0                                      Length of text
004600040310     D                                     INZ(%SIZE(SndMg03))
004700040310     D SndMg05         S             10                                         User profile
004800040310     D                                     DIM(299)
004900040310     D SndMg06         S             10I 0                                      Number of user
005000040310     D SndMg07         S             10I 0                                      Message sent indic.
005100040310     D SndMg08         S             10I 0                                      Function requested
005200040310     D SndMg10         S              1                                         Show display
005300040310     D                                     INZ('N')
005400040310     D SndMg11         S             20                                         Qualified MSGQ name
005500040310     D SndMg12         S              4                                         Name type indicator
005600040310     D                                     INZ('*USR')
005700040310      * indice di scihera
005800040310     D Jx              S              5I 0
005900040310      *
006000040310     D/COPY QSYSINC/QRPGLESRC,QUSEC
006100000120      *---------------------------------------------------------------------*
006200960711     IRRIFCSUM
006300960524     I              RRIFCSUM                    SUMDAT
006400000120      *---------------------------------------------------------------------*
006500000120      * Ciclo principale
006600000120      *---------------------------------------------------------------------*
006700040301      *  Parametrti ricevuti:
006800040301      *     Nrrec_a   = Num.Rel.Rec. della testata da cui partire (potebbero esserci
006900040301      *                 più testate/dettagli all'interno dello stesso Flat File
007000040301      *     Quale_Det = in quale dettaglio CZ reperire la nazione del mittente.
007100040301      *                 Solitamente lo si prende dal primo ma se ci fosse qualche
007200040301      *                 problema lo deve cercare sui successivi prendendo il primo
007300040301      *                 valido.
007400070104      *     Naz-Mitt  = è il parametro da restituire al TRTC79R per permettere di
007500040301      *                 trovare l'UNB corretto.
007600040302      *---------------------------------------------------------------------*
007700040301     c     *entry        plist
007800040301     c                   parm                    Nrec              9 0
007900040301     c                   parm                    Naz_mitt          3
008000040302     c                   parm                    Ok_Naz            1
008100040322     c                   parm                    User_msg         10
008200070213     c                   parm                    UNB_Parz         35
008300040302      *
008400040302      * Caricamento Tabella Partner esteri
008500040302     C                   Z-ADD     0             X                 3 0
008600040302     C                   MOVEL     'PT'          TABCOD
008700040302     C     TABCOD        CHAIN     EDTAB01L                           30
008800040302     C     *IN30         DOWEQ     '0'
008900040310     C     TABFLG        ifEQ      *BLANKS
009000040302     C                   ADD       1             X
009100040302     C                   MOVEL     TABKEY        CPT(X)
009200040302     C                   End
009300040302     C     TABCOD        READE     EDTAB01L                               30
009400040302     C                   END
009500040301     C*
009600040301     C* IMPOSTO A ZERO VARIABILI DI PGM
009700040301     C                   MOVEL     'N'           WFINE             1
009800040302     C                   MOVEL     'N'           OK_Naz            1
009900040301     C                   z-add     0             conta             3 0
010000070214     C                   z-add     0             contaTD00         3 0
010100040301      *
010200040302      *  Si posiziona legge e memorizza il Nome del Partner Mittente
010300040302     c                   exsr      X_primacosa
010400980609      *
010500960329      * Lettura fino a fine file
010600040302     C     Wfine         DOWNE     'S'                                          - Fine file -
010700070212      *
010800070212      * ?------------------- *
010900070214      * Se ha finito i dettaglio e legge una successiva testata senza avere
011000070214      *  trovato nulla deve uscire forzatamente
011100070214     C     SUMTPR        ifEQ      'TT00'
011200070214     c     conta         andGT     1
011300070214     C                   leave
011400070214     C                   endif
011500070214      *
011600070214      * se è al dettaglio successivo deve comunque uscire
011700070214     C     SUMTPR        ifeq      'TD00'
011800070214     C     contaTD00     ifgt      0
011900070214     c                   leave
012000070214     c                   else
012100070214     c                   add       1             contaTD00
012200070214     c                   end
012300070214     c                   end
012400980609      *
012500040301     C     SUMTPR        ifeq      'TD15'
012600040302      *               *--------------------*
012700980609     C                   EXSR      DRED15
012800040302      *               *--------------------*
012900040301      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
013000040302     c                   if        OK_Naz = 'S'
013100040301     C                   leave
013200040301     C                   endif
013300040301     C                   endif
013400980609      *
013500960329      * Lettura successiva
013600960329     C                   EXSR      REDFIL
013700960329     C                   END                                                    - Fine file -
013800980609      *
013900040301     c     FINE          tag
014000040310      *
014100040310      * Msg di allerta Traduzione da Flat file non finita bene
014200040310     c                   IF        Ok_Naz = 'N'
014300070213     c*******            exsr      SEND_MSG_EDP
014400040310     C                   END                                                    - Fine file -
014500040310      *
014600960329     C                   SETON                                        LR
014700960524     C*----------------------------------------------------------------
014800040302     C*? x Prima cosa: Si posiziona Legge e memorizza il nome generico del Partner
014900960524     C*----------------------------------------------------------------
015000040302     C     x_Primacosa   BEGSR
015100960524     C*
015200040302      * Posizionamento iniziale sul file
015300040302     C                   EXSR      SETFIL
015400040302      * Lettura sequenziale
015500040302     C                   EXSR      REDFIL
015600040302     C*
015700040302      * Imposta il codice Partner
015800070213     C                   MOVEL     UNB_Parz      WCODPT           35
015900070214     C                   MOVE      '    '        WCODPT
016000040302     C*
016100960524     C                   ENDSR
016200040302     C*----------------------------------------------------------------
016300040302     C*? SETFIL - POSIZIONAMENTO FRA più TESTATE
016400040302     C*----------------------------------------------------------------
016500040302     C     SETFIL        BEGSR
016600040302     C* Lettura file
016700040302     C     NREC          SETLL     EDIFCSUM
016800040302     C*
016900040302     C                   ENDSR
017000040301     C*----------------------------------------------------------------
017100040301     C*? REDFIL - LETTURA FILE
017200040301     C*----------------------------------------------------------------
017300040301     C     REDFIL        BEGSR
017400040301     C* Lettura file
017500040301     C                   READ      EDIFCSUM                               30
017600040301     C   30              MOVEL     'S'           WFINE
017700040301     C  N30              MOVEL     SUMDAT        WDAT
017800040301     C*
017900040301     C                   ENDSR
018000010502     C*----------------------------------------------------------------
018100960527     C*? DRED15 - DECODIFICA TIPO RECORD TD15
018200960527     C*----------------------------------------------------------------
018300960527     C     DRED15        BEGSR
018400000120      *
018500070219     C                   MOVEL     SUMDAT        EDTD15DS
018600980930      * Dati mittente
018700070219     C                   IF         DD3035 = 'CZ' and
018800070219     C                              DD3207 <> *blanks
018900040301     C                   add       1             conta
019000040302      *
019100040302      * Verifica esistenza su tab.:PT
019200070219     C                   eval      W035A = %subst(WCODPT:1:31) + DD3207
019300040302     C                   z-add     1             X
019400040302     C     W035a         lookup    CPT(X)                                 32
019500040302     C                   If        *in32 = *on
019600070219     c                   movel     DD3207        Naz_mitt
019700040302     C                   movel     'S'           OK_Naz
019800070214     c                   movel(p)  W035a         UNB_Parz
019900040302     c                   endIF
020000040302      *
020100000125     C                   ENDIF
020200031211      *
020300960527     C                   ENDSR
020400040310     c*==================================================================*
020500040310      * Manda un Msg a EDP*
020600040310     c*==================================================================*
020700040310     c     SEND_MSG_EDP  begsr
020800040310      *
020900040310      * Imposta la Frase del messaggio
021000040310     c                   clear                   msgDSP
021100070104     C                   EVAL      msgDSP  = 'ATTENZIONE:  TRTC79R non riesce a-
021200041207     C                              decod. il Cod.Partner da Tab.(PT). Il mitte-
021300041207     C                             nte ricevuto nell''IFCSUM è >'
021400041207     C                   EVAL      msgDSP  = %Trim(msgDSP) + WCODPT +
021500041207     C                             %Trim('< Controllare membri su TRIFCSUM o in-
021600041207     C                              RCVIFCSUM records con stato (ER)')
021700040310      *
021800040310      * INVIA MESSAGGIO ad un Utente --> EDPAB
021900040322     c                   if        User_msg <> *blank
022000040310     c                   z-add     1             Jx
022100040310     C                   exsr      CalQEZSNDMG
022200040322     c                   end
022300040310      *
022400040310     c                   endsr
022500040310     ***********************************************************************
022600040310     **
022700040310     ** Send Message (QEZSNDMG) API
022800040310     **
022900040310     ***********************************************************************
023000040310     C     CalQEZSNDMG   BEGSR
023100040310      *
023200040310     ** Invio messaggio all'utente.
023300040310     C                   EVAL      SndMg03 = msgDSP
023400040322     C******             EVAL      SndMg05(Jx) = 'EDPAB     '
023500040322     C                   EVAL      SndMg05(Jx) = User_msg
023600040310     C                   EVAL      SndMg06 = Jx
023700040310     C                   CLEAR                   QUSEC
023800040310     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
023900040310
024000040310     C                   CALL      'QEZSNDMG'
024100040310     C                   PARM                    SndMg01
024200040310     C                   PARM                    SndMg02
024300040310     C                   PARM                    SndMg03
024400040310     C                   PARM                    SndMg04
024500040310     C                   PARM                    SndMg05
024600040310     C                   PARM                    SndMg06
024700040310     C                   PARM                    SndMg07
024800040310     C                   PARM                    SndMg08
024900040310     C                   PARM                    Qusec
025000040310     C                   PARM                    SndMg10
025100040310     C                   PARM                    SndMg11
025200040310     C                   PARM                    SndMg12
025300040310      *
025400040310     C                   ENDSR
025500040310     O*****************************************************************
