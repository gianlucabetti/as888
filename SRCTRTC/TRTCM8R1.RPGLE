000100970526     H*------------------------------------------------------------------------*
000200060811     H* CONVERSIONE FILE DB2/400 IN EXCEL
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800050617     Ftntbe01l  IF   E           K DISK
000900110616     Ftimar01l  IF   E           K DISK
001000060811     FPRTEXCEL  O    F  378        PRINTER OFLIND(*INOF) USROPN
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D*------------------------------------------------------------------------*
001500970526     D* INPUT
001600970526     D*------------------------------------------------------------------------*
001700060811     D*------------------
001800060811     D* TABELLEA 'MRA'
001900060811     D*------------------
002000060811     D DMRAXLS       E DS
002100060811     D*------------------
002200060811     D* DS ridefinizione dati utente estesi spool x mailing
002300060811     D*------------------
002400060811     D TRTCM1DS      E DS
002500020923     D*-------------------
002600020923     D* DS REPERIMENTO DATI UTENTE
002700020923     D*-------------------
002800020923     D TIBS34DS      E DS                                                       *Profili utente
002900020923     D DDATIUTE      E DS                                                       *Dati utente
003000020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003100060811     D*------------------
003200060811     D* VARIABILI D WRK
003300060811     D*------------------
003400060811     D RigaStampa      s            378    inz
003500060811     D Esito           s              1    inz
003600060811     D wLibreria       s             10    inz
003700060811     D wFile           s             10    inz
003800060811     D wMembro         s             10    inz
003900060811     D wSelect         s            378    inz
004000060811     D wWhere          s            378    inz
004100060811     D wOrderBy        s            378    inz
004200060811     D wNomeFileXLS    s             50    inz
004300060811     D wPwdFileXLS     s             15    inz
004400970526     C*------------------------------------------------------------------------*
004500000303     C* MAIN LINES
004600970526     C*------------------------------------------------------------------------*
004700110616     C*
004800110616     C                   EVAL      *in55 = *off
004900110616     C*
005000110616     C                   EXSR      REPMRA
005100110616     C                   EXSR      ESEGUI
005200110616     C*
005300110616     C                   EVAL      pOutESITO = *in55
005400970526     C*
005500970526     C* FINE PGM
005600970526     C                   SETON                                        LR
005700991214     C*------------------------------------------------------------------------*
005800060811     C* REPERIMENTO REGOLA DA TABELLA "MRA"
005900060811     C*------------------------------------------------------------------------*
006000060811     C     REPMRA        BEGSR
006100060811     C*
006200060811     C* Reperimento tabella MRA
006300060811     C                   CLEAR                   DMRAXLS
006400060811     C                   MOVE(P)   'MRA'         tbeCOD
006500110616     C                   MOVEL(p)  pInFILE       tbeKE1
006600120504     C                   MOVEL     wMRAKE2       tbeKE2
006700060811     C     KEYtbe1_P     CHAIN     TNTBE01L
006800060811     C                   IF        %found(TNTBE01L) AND
006900060811     C                             tbeATB <> 'A'
007000060811     C                   MOVEL     tbeUNI        DMRAXLS
007100110616     C                   ELSE
007200110616     C                   SETON                                        55
007300060811     C                   ENDIF
007400060905     C*
007500060905     C* Se tutto ok reperisco gli attributi regola dal file regole BartMailing
007600110616     C                   IF        not *IN55
007700060905     C     冶RAREG       CHAIN     timar01l
007800060905     C                   IF        %found(timar01l)
007900060905     C*
008000110616     C* Se presente su regola nome file lo imposto come default
008100060905     C                   IF        MARFOUT <> *blanks
008200060905     C                   EVAL      wNomeFileXLS = MARFOUT
008300060905     C                   ENDIF
008400060905     C                   ELSE
008500110616     C                   SETON                                        55
008600060905     C                   ENDIF
008700060905     C                   ENDIF
008800060811     C*
008900060811     C                   ENDSR
009000970526     C*------------------------------------------------------------------------*
009100991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
009200970526     C*------------------------------------------------------------------------*
009300991214     C     ESEGUI        BEGSR
009400060811     C*
009500060811     C* Reperisco la libreria in cui risiede il file richiesto
009600060811     C                   CALL      'TRTCM8C'
009700060811     C                   PARM      *blanks       wLibreria
009800110616     C                   PARM      pInFILE       wFile
009900060811     C*
010000060811     C* Imposto quindi i dati utenti estesi
010100060811     C                   CLEAR                   TRTCM1DS
010200060811     C                   EVAL      低M1TIPS = 冶RAREG
010300060811     C                   EVAL      低M1IDP  = 冶RAIDPRO
010400060811     C                   MOVEL(P)  DUTPOU        低M1PO
010500110617     C                   IF        pInEMLDEST <> *blanks
010600110617     C                   EVAL      低M1MITT = 'ced'
010700110616     C                   EVAL      低M1DST  = pInEMLDEST
010800110617     C                   ENDIF
010900111202     C                   EVAL      低M1VAR  = pInEMLVAR
011000060811     C*
011100060811     C* Eseguo override x generare uno spool ad "hoc"
011200060811     C                   CALL      'TRUL83C1'
011300060811     C                   PARM      'P'           OPZIONE           1
011400060811     C                   PARM      'PRTEXCEL'    PRTFILE          10
011500060811     C                   PARM                    TRTCM1DS
011600060811     C                   PARM                    冶RAOUTQI
011700060811     C                   PARM                    Esito             1
011800060811     C*
011900060811     C* Valorizzo le variabili d definizione stringa SQL
012000110616     C                   EVAL      wSelect = '*'
012100060811     C                   EVAL      wWhere = %trim(冶RAFLDID) + ' = ' + '''' +
012200110616     C                                      %trim(pInIDJOB)  + ''''
012300060811     C*
012400060811     C* Creo lo spool
012500060811     C                   OPEN      PRTEXCEL
012600060811     C*
012700060811     C* - Prima riga: Libreria AS400
012800060822     C                   EVAL      RigaStampa = '1'+wLibreria
012900060811     C                   EXCEPT    STA_RIGA
013000060811     C*
013100060811     C* - Seconda riga: File AS400
013200060822     C                   EVAL      RigaStampa = '2'+wFile
013300060811     C                   EXCEPT    STA_RIGA
013400060811     C*
013500060811     C* - Terza riga: Membro AS400
013600060822     C                   EVAL      RigaStampa = '3'+wMembro
013700060811     C                   EXCEPT    STA_RIGA
013800060811     C*
013900060811     C* - Quarta riga: SELECT
014000060822     C                   EVAL      RigaStampa = '4'+wSelect
014100060811     C                   EXCEPT    STA_RIGA
014200060811     C*
014300060811     C* - Quinta riga: WHERE
014400060822     C                   EVAL      RigaStampa = '5'+wWhere
014500060811     C                   EXCEPT    STA_RIGA
014600060811     C*
014700060811     C* - Sesta riga: ORDER BY
014800060822     C                   EVAL      RigaStampa = '6'+wOrderBy
014900060811     C                   EXCEPT    STA_RIGA
015000060811     C*
015100060811     C* - Settima riga: Nome file Excel
015200110617     C                   IF        %trim(wNomeFileXLS) = '*AUTO*'
015300060822     C                   EVAL      wNomeFileXLS = %trim(wFile) + '_' +
015400110616     C                                            %trim(pInIDJOB)
015500060822     C                   ENDIF
015600060822     C                   EVAL      RigaStampa = '7'+wNomeFileXLS
015700060811     C                   EXCEPT    STA_RIGA
015800060811     C*
015900060811     C* - Ottava riga: Password file Excel
016000060905     C                   IF        wPwdFileXLS = *blanks
016100110616     C                   EVAL      wPwdFileXLS = %trim(pInPWDXLS)
016200060905     C                   ENDIF
016300060822     C                   EVAL      RigaStampa = '8'+wPwdFileXLS
016400060811     C                   EXCEPT    STA_RIGA
016500060811     C*
016600060811     C* Chiudo lo spool
016700060811     C                   CLOSE     PRTEXCEL
016800060811     C*
016900060811     C* Eseguo delete override precedente
017000060811     C                   CALL      'TRUL83C1'
017100060811     C                   PARM      'D'           OPZIONE
017200060811     C                   PARM                    PRTFILE
017300060811     C                   PARM                    TRTCM1DS
017400060811     C                   PARM                    冶RAOUTQI
017500060811     C                   PARM                    Esito
017600991214     C*
017700970526     C                   ENDSR
017800020923     C*--------------------------------------------------------------------------------------------*
017900020923     C* REPERISCE I DATI UTENTE
018000020923     C*--------------------------------------------------------------------------------------------*
018100020923     C     REPDATIUTE    BEGSR
018200020923     C*
018300020923     C* INIZIALIZZA VARIABILI DI WRK
018400020923     C                   CLEAR                   TIBS34DS
018500020923     C                   CLEAR                   AZUTEDS
018600020923     C                   CLEAR                   DDATIUTE
018700020923     C*
018800020923     C     *DTAARA       DEFINE    吧zute        azuteds
018900020923     C     *DTAARA       DEFINE    吳atiute      ddatiute
019000020923     C                   IN(E)     *DTAARA
019100110616     C                   IF        %Error
019200020923     c                   EVAL      I34Tla = 'L'
019300020923     C                   CALL      'TIBS34R'
019400020923     C                   PARM                    Tibs34Ds
019500020923     C                   IN        *DTAARA
019600110616     C                   ENDIF
019700020923     C*
019800020923     C                   ENDSR
019900970526     C*------------------------------------------------------------------------*
020000970526     C* *INZSR - OPERAZIONI INIZIALI
020100970526     C*------------------------------------------------------------------------*
020200970526     C     *INZSR        BEGSR
020300991217     C*
020400110616     C     *ENTRY        PLIST
020500110616     C                   PARM                    pInFILE          10
020600110616     C                   PARM                    pInIDJOB         26
020700110616     C                   PARM                    pInPWDXLS        15
020800110616     C                   PARM                    pINNOMXLS        78
020900110616     C                   PARM                    pInEMLDEST      121
021000111202     C                   PARM                    pInEMLVAR        80
021100120504     C                   PARM                    pInMRAKE2        15
021200110616     C                   PARM                    pOutESITO         1
021300120504     C*
021400120504     C* Verifico parametri in ingresso
021500120504     C                   if        pInMRAKE2 <> *blanks
021600120504     C                   movel     pInMRAKE2     wMRAKE2          15
021700120504     C                   else
021800120504     C                   movel     'XLS'         wMRAKE2
021900120504     C                   endif
022000020923     C*
022100020923     C* REPERISCE I DATI DELL'UTENTE
022200020923     C                   EXSR      REPDATIUTE
022300060811     C*
022400050617     C* Definizione chiavi - TNTBE01L
022500060811     C     KEYtbe1_P     KLIST
022600050617     C                   KFLD                    tbeCOD
022700050617     C                   KFLD                    tbeKE1
022800060811     C                   KFLD                    tbeKE2
022900970526     C*
023000970526     C                   ENDSR
023100060811     O*------------------------------------------------------------------------*
023200060811     O* STAMPA
023300060811     O*------------------------------------------------------------------------*
023400060811     OPRTEXCEL  E            STA_RIGA    1
023500060811     O                       RigaStampa
023600060811     O*------------------------------------------------------------------------*
