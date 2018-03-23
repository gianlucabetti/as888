000100010110      /TITLE ENDJRNPF/STRJRNPF e RMVPFTRG/ADDPFTRG di un file.
000200010110
000300010110      * ------------------------------------------------------------------ *
000400010110      *
000500010110      * Questo programma esegue:
000600010110      *
000700010110      * 1a) ENDJRNPF di un file.
000800010110      *
000900010110      * oppure
001000010110      *
001100010110      * 1b) STRJRNPF di un file
001200010110      *     reperendo i parametri dal file stesso,
001300010110      *     quindi il file deve essere già stato registrato sul giornale
001400010110      *     almeno una volta.
001500010110      *
001600010110      * e/o
001700010110      *
001800010110      * 2a) RMVPFTRG di tutti i trigger di un file.
001900010110      *
002000010110      * oppure
002100010110      *
002200010110      * 2b) ADDPFTRG chiamando il programma specifico associato al file.
002300010110      *
002400010110      * ------------------------------------------------------------------ *
002500010110      *
002600010110      * Parametri di input:
002700010110      *
002800010110      * PRMFILE    Obbligatorio
002900010110      *            Nome del file fisico.
003000010110      * PRMLIB     Facoltativo
003100010110      *            Nome della libreria del file fisico.
003200010110      * PRMJRNPF   Esecuzione comando giornale
003300010110      *            ' ' = nessun comando.
003400010110      *            'E' = comando ENDJRNPF.
003500010110      *            'S' = comando STRJRNPF.
003600010110      * PRMPFTRG   Esecuzione comando trigger
003700010110      *            ' ' = nessun comando.
003800010110      *            'R' = comando RMVPFTRG.
003900010110      *            'A' = comando ADDPFTRG.
004000010112      * PRMALCDLC  (Dis)allocazione file.
004100010112      *            ' ' = nessun comando.
004200010112      *            'A' = comando ALCOBJ.
004300010112      *            'D' = comando DLCOBJ.
004400010110      *
004500010110      * Parametri di outuput:
004600010110      *
004700010110      * PRMESITO   Esito elaborazione
004800010110      *            '0' = eseguito.
004900010110      *            '1' = errore.
005000010112      * PRMMSGID   MSGID errore.
005100010110      *
005200010110      * ------------------------------------------------------------------ *
005300010110      *
005400010110      * Esempi di chiamata:
005500010110      *
005600010110      * ENDJRNPF e RMVPFTRG del file ANIND00F.
005700010110      *
005800010110      *                  CALL      'TRUL17R'
005900010110      *                  PARM      'ANIND00F'    PrmFile
006000010110      *                  PARM      '*LIBL'       PrmLib
006100010110      *                  PARM      'E'           PrmJrnPf
006200010110      *                  PARM      'R'           PrmPfTrg
006300010112      *                  PARM      'A'           PrmAlcDlc
006400010110      *                  PARM      *BLANK        PrmEsito
006500010112      *                  PARM      *BLANK        PrmMsgId
006600010110      *
006700010110      * STRJRNPF e ADDPFTRG del file ANIND00F.
006800010110      *
006900010110      *                  CALL      'TRUL17R'
007000010110      *                  PARM      'ANIND00F'    PrmFile
007100010110      *                  PARM      '*LIBL'       PrmLib
007200010110      *                  PARM      'S'           PrmJrnPf
007300010110      *                  PARM      'A'           PrmPfTrg
007400010112      *                  PARM      'D'           PrmAlcDlc
007500010110      *                  PARM      *BLANK        PrmEsito
007600010112      *                  PARM      *BLANK        PrmMsgId
007700010110      *
007800010110      * ------------------------------------------------------------------ *
007900010109
008000010109     H DFTACTGRP(*NO) ACTGRP(*CALLER)
008100010109
008200010109     D StrJrnPf        C                   'S'
008300010109     D EndJrnPf        C                   'E'
008400010109     D AddPfTrg        C                   'A'
008500010109     D RmvPfTrg        C                   'R'
008600010112     D AlcObj          C                   'A'
008700010112     D DlcObj          C                   'D'
008800010109     D Eseguito        C                   '0'
008900010109     D Errore          C                   '1'
009000010110      * Definizione paramatri.
009100010109     D PrmFile         S             10
009200010109     D PrmLib          S             10
009300010110     D PrmJrnPf        S              1
009400010110     D PrmPfTrg        S              1
009500010112     D PrmAlcDlc       S              1
009600010109     D PrmEsito        S              1
009700010112     D PrmMsgId        S
009800010112     D                                     LIKE(Qusei)
009900010109      * Parametri API QCAPCMD.
010000010109     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
010100010109     D PCmd01          S            256
010200010109     D PCmd02          S              9B 0
010300010109     D                                     INZ(%SIZE(PCmd01))
010400010109     D PCmd04          S              9B 0
010500010109     D                                     INZ(%SIZE(Qcap0100))
010600010109     D PCmd05          S              8
010700010109     D                                     INZ('CPOP0100')
010800010109     D PCmd06          S
010900010109     D                                     LIKE(PCmd01)
011000010109     D PCmd07          S              9B 0
011100010109     D                                     INZ(%SIZE(PCmd06))
011200010109     D PCmd08          S              9B 0
011300010109      * Parametri API QDBRTVFD.
011400010109     D/COPY QSYSINC/QRPGLESRC,QDBRTVFD
011500010109     D Qdbrv           S           2048
011600010110     D Qdbfjbim        S              1
011700010110     D Qdbfjomt        S              1
011800010109      * Parametro gestione errori API.
011900010109     D/COPY QSYSINC/QRPGLESRC,QUSEC
012000010109
012100010109     C     *ENTRY        PLIST
012200010109     C                   PARM                    PrmFile
012300010109     C                   PARM                    PrmLib
012400010109     C                   PARM                    PrmJrnPf
012500010109     C                   PARM                    PrmPfTrg
012600010112     C                   PARM                    PrmAlcDlc
012700010109     C                   PARM                    PrmEsito
012800010112     C                   PARM                    PrmMsgId
012900010109      * Controllo i parametri ricevuti.
013000010109     C                   EXSR      ChkParm
013100010110      * Reperisco la descrizione del file.
013200010110     C                   EXSR      RtvFd
013300010112      * Allocazione file.
013400010112     C                   EXSR      AlcFile
013500010110      * Esecuzione comandi giornale.
013600010110     C                   EXSR      Giornale
013700010110      * Esecuzione comandi trigger.
013800010110     C                   EXSR      Trigger
013900010112      * Disallocazione file.
014000010112     C                   EXSR      DlcFile
014100010110      *
014200010110     C                   EXSR      Uscita
014300010109
014400010110     C     Giornale      BEGSR
014500010110      *
014600010110     C                   IF        PrmJrnPf <> *BLANK
014700010110      *
014800010109     C                   SELECT
014900010109      * E' stato chiesto ENDJRNPF
015000010109      * e il file è attualmente registrato sul giornale.
015100010109     C                   WHEN      PrmJrnPf = EndJrnPf
015200010109     C                             AND
015300010109     C                             Qdbfjact = *ON
015400010109     C                   EVAL      PCmd01 = 'ENDJRNPF'
015500010109     C                             +
015600010109     C                             ' FILE('
015700010109     C                             + %TRIMR(PrmLib)  + '/'
015800010109     C                             + %TRIMR(PrmFile) + ')'
015900010109      * E' stato chiesto STRJRNPF
016000010109      * e il file attualmente non è registrato sul giornale
016100010109      * ma lo è stato, quindi lo avvio sul precedente giornale.
016200010109     C                   WHEN      PrmJrnPf = StrJrnPf
016300010109     C                             AND
016400010109     C                             Qdbfjact = *OFF
016500010109     C                             AND
016600010109     C                             Qdbfojrn <> *BLANK
016700010109      *
016800010109     C                   EVAL      PCmd01 = 'STRJRNPF'
016900010109     C                             +
017000010109     C                             ' FILE('
017100010109     C                             + %TRIMR(PrmLib) + '/'
017200010109     C                             + %TRIMR(PrmFile)  + ')'
017300010109     C                             +
017400010109     C                             ' JRN('
017500010109     C                             + %TRIMR(Qdbfolib) + '/'
017600010109     C                             + %TRIMR(Qdbfojrn) + ')'
017700010110      * C'erano anche le immagini before.
017800010110     C                   IF        Qdbfjbim = *ON
017900010109     C                   EVAL      PCmd01 = %TRIMR(PCmd01)
018000010109     C                             + ' IMAGES(*BOTH)'
018100010110     C                   ENDIF
018200010110      * Voci giornale da omettere.
018300010110     C                   IF        Qdbfjomt = *ON
018400010109     C                   EVAL      PCmd01 = %TRIMR(PCmd01)
018500010109     C                             + ' OMTJRNE(*OPNCLO)'
018600010110     C                   ENDIF
018700010109      *
018800010109     C                   ENDSL
018900010110      *
019000010110     C                   CLEAR                   Qusec
019100010110     C                   EXSR      CalQCAPCMD
019200010110      *
019300010110     C                   ENDIF
019400010110      *
019500010110     C                   ENDSR
019600010109
019700010110     C     Trigger       BEGSR
019800010110      *
019900010110     C                   IF        PrmPfTrg <> *BLANK
020000010110      *
020100010110     C                   SELECT
020200010110      * E' stato chiesto di rimuovere i trigger
020300010110      * e ci sono dei trigger.
020400010110     C                   WHEN      PrmPfTrg = RmvPfTrg
020500010110     C                             AND
020600010110     C                             Qdbftrgn > 0
020700010110      *
020800010109     C                   EVAL      PCmd01 = 'RMVPFTRG'
020900010109     C                             +
021000010109     C                             ' FILE('
021100010109     C                             + %TRIMR(PrmLib) + '/'
021200010109     C                             + %TRIMR(PrmFile)  + ')'
021300010109     C                   CLEAR                   Qusec
021400010109     C                   EXSR      CalQCAPCMD
021500010110      * E' stato chiesto di aggiungere i trigger
021600010110      * e non ci sono dei trigger.
021700010110     C                   WHEN      PrmPfTrg = AddPfTrg
021800010110     C                             AND
021900010110     C                             Qdbftrgn = 0
022000010110      * Chiamo il programma di aggiunta trigger.
022100010110     C                   SELECT
022200010110     C                   WHEN      PrmFile = 'NDMOV00F'
022300010110     C                   CALL      'NDMOVT'
022400010110     C                   WHEN      PrmFile = 'NDBMV00F'
022500010110     C                   CALL      'NDBMV'
022600010110     C                   WHEN      PrmFile = 'NDMOA00F'
022700010110     C                   CALL      'NDMOA'
022800010110     C                   WHEN      PrmFile = 'ANIND00F'
022900010110     C                   CALL      'ANIND'
023000010110     C                   ENDSL
023100010110      *
023200010110     C                   ENDSL
023300010110      *
023400010110     C                   ENDIF
023500010110      *
023600010110     C                   ENDSR
023700010110
023800010110      * Reperisco la descrizione del file.
023900010110     C     RtvFd         BEGSR
024000010110      *
024100010110     C                   CLEAR                   Qusec
024200010110     C                   EVAL      Qdblorv = %SIZE(Qdbrv)
024300010110     C                   EVAL      Qdbfaln = PrmFile + PrmLib
024400010110     C                   CALL      'QDBRTVFD'
024500010110     C                   PARM      *BLANK        Qdbrv
024600010110     C                   PARM                    Qdblorv
024700010110     C                   PARM      *BLANK        Qdbrfal
024800010110     C                   PARM      'FILD0100'    Qdbfn00
024900010110     C                   PARM                    Qdbfaln
025000010110     C                   PARM      '*FIRST'      Qdbrfn00
025100010110     C                   PARM      '0'           Qdbfilof
025200010110     C                   PARM      '*LCL'        Qdbystem
025300010110     C                   PARM      '*INT'        Qdbft
025400010110     C                   PARM                    Qusec
025500010110      * Reperisco le informazioni di testata.
025600010110     C                   EVAL      Qdbq25 = Qdbrv
025700010110      * Reperisco le informazioni del PF.
025800010110     C                   EVAL      Qdbq26 = %SUBST(Qdbrv:Qdbpfof+1)
025900010110      * Reperisco le informazioni del giornale.
026000010110     C                   EVAL      Qdbq40 = %SUBST(Qdbrv:Qdbfjorn+1)
026100010110      * Controllo il 2o bit per sapere se ci sono anche le immagini before.
026200010110     C                   TESTB     '1'           QdbBits41                01
026300010110     C                   EVAL      Qdbfjbim = *IN01
026400010110      * Controllo il 5o bit per sapere il parametro OMTJRNE.
026500010110     C                   TESTB     '4'           QdbBits41                01
026600010110     C                   EVAL      Qdbfjomt = *IN01
026700010110      *
026800010110     C                   ENDSR
026900010109
027000010109      * Esecuzione comando.
027100010109     C     CalQCAPCMD    BEGSR
027200010109      *
027300010109     C                   CLEAR                   Qcap0100
027400010109     C                   EVAL      Qcabcsdh = '0'
027500010109     C                   EVAL      Qcapa = '0'
027600010109     C                   EVAL      Qcacmdss = '0'
027700010109     C                   EVAL      Qcaerved = X'000000000000000000'
027800010109      *
027900010109     C                   CALL      'QCAPCMD'
028000010109     C                   PARM                    PCmd01
028100010109     C                   PARM                    PCmd02
028200010109     C                   PARM                    Qcap0100
028300010109     C                   PARM                    PCmd04
028400010109     C                   PARM                    PCmd05
028500010109     C                   PARM      *BLANK        PCmd06
028600010109     C                   PARM                    PCmd07
028700010109     C                   PARM                    PCmd08
028800010109     C                   PARM                    Qusec
028900010109      *
029000010109     C                   ENDSR
029100010109
029200010109      * Controllo parametri ricevuti.
029300010109     C     ChkParm       BEGSR
029400010109      *
029500010109     C                   CLEAR                   PrmEsito
029600010112     C                   CLEAR                   PrmMsgId
029700010110      * Parametri obbligatori.
029800010112     C                   IF        PrmFile = *BLANK
029900010109     C                   EVAL      PrmEsito = Errore
030000010109     C                   EXSR      Uscita
030100010109     C                   ENDIF
030200010110      * Se non ricevo la libreria assumo *LIBL.
030300010109     C                   IF        PrmLib = *BLANK
030400010109     C                   EVAL      PrmLib = '*LIBL'
030500010109     C                   ENDIF
030600010110      * Controllo l'esistenza del file.
030700010110     C                   EVAL      PCmd01 = 'CHKOBJ'
030800010110     C                             +
030900010110     C                             ' OBJ('
031000010110     C                             + %TRIMR(PrmLib)  + '/'
031100010110     C                             + %TRIMR(PrmFile) + ')'
031200010110     C                             +
031300010110     C                             ' OBJTYPE(*FILE)'
031400010110     C                   CLEAR                   Qusec
031500010110     C                   EVAL      Qusbprv = %SIZE(Qusec)
031600010110     C                   EXSR      CalQCAPCMD
031700010110      * Il file non esiste.
031800010110     C                   IF        Qusei <> *BLANK
031900010110     C                   EVAL      PrmEsito = Errore
032000010112     C                   EVAL      PrmMsgId = Qusei
032100010110     C                   EXSR      Uscita
032200010110     C                   ENDIF
032300010109      *
032400010109     C                   ENDSR
032500010112
032600010112      * Allocazione file.
032700010112     C     AlcFile       BEGSR
032800010112      *
032900010112     C                   IF        PrmAlcDlc = AlcObj
033000010112      *
033100010112     C                   EVAL      PCmd01 = 'ALCOBJ'
033200010112     C                             + ' OBJ(('
033300010112     C                             + %TRIMR(PrmLib) + '/' + %TRIMR(PrmFile)
033400010112     C                             + ' *FILE *EXCLRD))'
033500010112     C                   CLEAR                   Qusec
033600010112     C                   EVAL      Qusbprv = %SIZE(Qusec)
033700010112     C                   EXSR      CalQCAPCMD
033800010112      * Comando non riuscito.
033900010112     C                   IF        Qusei <> *BLANK
034000010112     C                   EVAL      PrmEsito = Errore
034100010112     C                   EVAL      PrmMsgId = Qusei
034200010112     C                   EXSR      Uscita
034300010112     C                   ENDIF
034400010112      *
034500010112     C                   ENDIF
034600010112      *
034700010112     C                   ENDSR
034800010112
034900010112      * Disallocazione file.
035000010112     C     DlcFile       BEGSR
035100010112      *
035200010112     C                   IF        PrmAlcDlc = DlcObj
035300010112      *
035400010112     C                   EVAL      PCmd01 = 'DLCOBJ'
035500010112     C                             + ' OBJ(('
035600010112     C                             + %TRIMR(PrmLib) + '/' + %TRIMR(PrmFile)
035700010112     C                             + ' *FILE *EXCLRD))'
035800010112     C                   CLEAR                   Qusec
035900010112     C                   EVAL      Qusbprv = %SIZE(Qusec)
036000010112     C                   EXSR      CalQCAPCMD
036100010112      * Comando non riuscito.
036200010112     C                   IF        Qusei <> *BLANK
036300010112     C                   EVAL      PrmEsito = Errore
036400010112     C                   EVAL      PrmMsgId = Qusei
036500010112     C                   EXSR      Uscita
036600010112     C                   ENDIF
036700010112      *
036800010112     C                   ENDIF
036900010112      *
037000010112     C                   ENDSR
037100010109
037200010109      * Uscita.
037300010109     C     Uscita        BEGSR
037400010109      *
037500010109     C                   IF        PrmEsito = *BLANK
037600010109     C                   EVAL      PrmEsito = Eseguito
037700010109     C                   ENDIF
037800010109      *
037900010109     C                   RETURN
038000010109      *
038100010109     C                   ENDSR
