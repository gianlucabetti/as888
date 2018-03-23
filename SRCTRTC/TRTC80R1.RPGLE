000100980618      ***********************************************************************
000200040301      *   Chiamato da TRTC80R                        - E.D.I.   ESTERO -    *
000300960329      ***********************************************************************
000400040301 :    * Serve x trascodificare la tabella PT tramite l'UNB di codici dubbi
000500040301      ***********************************************************************
000600040301 :    * es.: NLLYN che può avere + LNP e sulla tabella PT viene codificato
000700040301 :    * più volte esempio: " NLLYN ...... NL " e " NLLYN ...... BE "
000800040301 :    * poichè gestisce 2 nazioni Belgio e Olanda
000900000126      ***********************************************************************
001000040301 :    * Il programma serve per reperire la Nazione del primo dettaglio mittente
001100040301 :    * dallo stesso ciclo di lettura del Flat file TRIFCSUM senza farlo nel
001200040301 :    * TRTC80R direttamente. Ciò sposizionerebbe il ciclo nel TRTC80R...così no.
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
002400040302     D EDTT00DS      E DS
002500000120     D EDTD15DS      E DS
002600000120     D  D15                  445    500    DIM(2)
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
007600040301      *     Naz-Mitt  = è il parametro da restituire al TRTC80R per permettere di
007700040301      *                 trovare l'UNB corretto.
007800040301      *---------------------------------------------------------------------*
007900040302     C     *LIKE         DEFINE    TA0004        WCODPT
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
010900980609      *
011000040301     C     SUMTPR        ifeq      'TD15'
011100040302      *               *--------------------*
011200980609     C                   EXSR      DRED15
011300040302      *               *--------------------*
011400040301      * deve fare uscire forzatamente dal ciclo appena ha l'informazione richiesta
011500040302     c                   if        OK_Naz = 'S'
011600040301     C                   leave
011700040301     C                   endif
011800040301     C                   endif
011900980609      *
012000040301      * Se ha finito i dettaglio e legge una successiva testata senza avere
012100040301      *  trovato nulla deve uscire forzatamente
012200040302     C     SUMTPR        ifEQ      'TT00'
012300040302     c     conta         andGT     1
012400040301     C                   leave
012500040301     C                   endif
012600960329      * Lettura successiva
012700960329     C                   EXSR      REDFIL
012800960329     C                   END                                                    - Fine file -
012900980609      *
013000040301     c     FINE          tag
013100040310      *
013200040310      * Msg di allerta Traduzione da Flat file non finita bene
013300040310     c                   IF        Ok_Naz = 'N'
013400040310     c                   exsr      SEND_MSG_EDP
013500040310     C                   END                                                    - Fine file -
013600040310      *
013700960329     C                   SETON                                        LR
013800960524     C*----------------------------------------------------------------
013900040302     C*? x Prima cosa: Si posiziona Legge e memorizza il nome generico del Partner
014000960524     C*----------------------------------------------------------------
014100040302     C     x_Primacosa   BEGSR
014200960524     C*
014300040302      * Posizionamento iniziale sul file
014400040302     C                   EXSR      SETFIL
014500040302      * Lettura sequenziale
014600040302     C                   EXSR      REDFIL
014700040302     C*
014800040302      * Imposta il codice Partner
014900040302     C     SUMTPR        ifeq      'TT00'
015000040302     C*
015100040302     C                   MOVEL     SUMDAT        EDTT00DS
015200040302     C                   MOVEL     TA0004        WCODPT
015300040302     C*
015400040302     C                   else
015500040302     C*
015600040302     C* Se il primo record letto non è la testata Partner UNB
015700040302     C*  deve uscire dal pgm senza dare nessun esito
015800040302     c                   move      'S'           Wfine
015900040302     C*
016000040302     C                   endif
016100040302     C*
016200960524     C                   ENDSR
016300040302     C*----------------------------------------------------------------
016400040302     C*? SETFIL - POSIZIONAMENTO FRA più TESTATE
016500040302     C*----------------------------------------------------------------
016600040302     C     SETFIL        BEGSR
016700040302     C* Lettura file
016800040302     C     NREC          SETLL     EDIFCSUM
016900040302     C*
017000040302     C                   ENDSR
017100040301     C*----------------------------------------------------------------
017200040301     C*? REDFIL - LETTURA FILE
017300040301     C*----------------------------------------------------------------
017400040301     C     REDFIL        BEGSR
017500040301     C* Lettura file
017600040301     C                   READ      EDIFCSUM                               30
017700040301     C   30              MOVEL     'S'           WFINE
017800040301     C  N30              MOVEL     SUMDAT        WDAT
017900040301     C*
018000040301     C                   ENDSR
018100010502     C*----------------------------------------------------------------
018200960527     C*? DRED15 - DECODIFICA TIPO RECORD TD15
018300960527     C*----------------------------------------------------------------
018400960527     C     DRED15        BEGSR
018500000120      *
018600000120     C                   MOVEL     SUMDAT        EDTD15DS
018700980930      * Dati mittente
018800040301     C                   IF         DD3035 = 'CZ' and
018900040301     C                              DD3207 <> *blanks
019000040301     C                   add       1             conta
019100040302      *
019200040302      * Verifica esistenza su tab.:PT
019300040302     C                   eval      W035A = %subst(WCODPT:1:31) + DD3207
019400040302     C                   z-add     1             X
019500040302     C     W035a         lookup    CPT(X)                                 32
019600040302     C                   If        *in32 = *on
019700040302     c                   movel     DD3207        Naz_mitt
019800040302     C                   movel     'S'           OK_Naz
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
021100040310     C                   EVAL      msgDSP  = 'ATTENZIONE:  TRTC80R non riesce a-
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
