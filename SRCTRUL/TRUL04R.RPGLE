000100001222      /TITLE Restituisce la libreria GAI partendo da quella di PROJ.
000200001222
000300001222      *--------------------------------------------------------------------*
000400001222      *
000500001222      * Parametri di input:
000600001222      *
000700001222      * PRMSOCIETA Codice società di PROJ.
000800001222      *            Obbligatorio.
000900001222      *            Può essere una società del ramo
001000001222      *            Bartolini ('2XX')
001100001222      *            o     SDI ('1XX'').
001200001222      * PRMLIBPROJ Libreria dati di PROJ.
001300001222      *            Obbligatorio.
001400001222      *            Può essere PJBAR* o PJPRV*.
001500001222      * PRMOBJTRA  Nome oggetto gestionale.
001600001222      *            Facoltativo.
001700001222      *            Se ricevuto e non esiste nella libreria gestionale,
001800021001      *            si cambia la libreria in quella alternativa 1.
001900001222      * PRMOBJTYPE Tipo oggetto.
002000001222      *            Obbligatorio se PRMOBJTRA è pieno.
002100001222      *            (*FILE, *DTAARA, ...)
002200010322      * PRMUNITA   Unità.
002300010322      *            Facoltativo.
002400001222      *
002500001222      * Parametri di output:
002600001222      *
002700001222      * PRMLIBTRA  E' la libreria gestionale desunta con la regola:
002800001222      *
002900021001      * PRMLIBPROJ PRMSOCIETA PRMUNITA PRMLIBTRA  Alternat.1 Alternat.2
003000021001      * ---------- ---------- -------- ---------- ---------- ----------
003100021001      * PJBAR*        1XX              SDITRA1XX  GAITRAGRU  UNITRAGRU
003200021001      * PJBAR*        2XX              GAITRA2XX  GAITRAGRU  UNITRAGRU
003300021001      * PJPRV*        1XX              SDITRAPRV  GAITRAGRPS UNITRAGRP
003400021001      * PJPRV*        2XX              GAITRAPRV  GAITRAGRPS UNITRAGRP
003500021001      * PJBAR*        201     2*       SDITRA101  GAITRAGRU  UNITRAGRU
003600021001      * PJPRV*        201     2*       SDITRAPRV  GAITRAGRPS UNITRAGRP
003700001222      *
003800001222      * PRMESITO   '0' = Eseguito.
003900001222      *            '1' = Errore.
004000001222      *
004100001222      * Esempio di chiamata:
004200001222      *
004300001222      *    Output                  Input
004400001222      *    ----------              ----------
004500001222      *                  CALL      'TRUL04R'
004600001222      *                  PARM      '201'         PrmSocieta
004700001222      *                  PARM      'PJBARGRU'    PrmLibProj
004800001222      *                  PARM      *BLANK        PrmObjTra
004900001222      *                  PARM      *BLANK        PrmObjType
005000001222      *    'GAITRA201'   PARM      *BLANK        PrmLibTra
005100001222      *    '0'           PARM      *BLANK        PrmEsito
005200001222      *
005300001222      *                  CALL      'TRUL04R'
005400001222      *                  PARM      '101'         PrmSocieta
005500001222      *                  PARM      'PJPRVSOC'    PrmLibProj
005600001222      *                  PARM      'AZORG00F'    PrmObjTra
005700001222      *                  PARM      '*FILE'       PrmObjType
005800001222      *    'GAITRAGRP'   PARM      *BLANK        PrmLibTra
005900001222      *    '0'           PARM      *BLANK        PrmEsito
006000010322      *
006100010322      *                  CALL      'TRUL04R'
006200010322      *                  PARM      '201'         PrmSocieta
006300010322      *                  PARM      'PJBARGRU'    PrmLibProj
006400010322      *                  PARM      'CNACO00F'    PrmObjTra
006500010322      *                  PARM      '*FILE'       PrmObjType
006600010322      *    'SDITRA101'   PARM      *BLANK        PrmLibTra
006700010322      *    '0'           PARM      *BLANK        PrmEsito
006800010322      *                  PARM      '202'         PrmUnita
006900001222      *
007000001222      *--------------------------------------------------------------------*
007100001222
007200001222     H DFTACTGRP(*NO) ACTGRP(*CALLER)
007300001222
007400001222     D Eseguito        C                   '0'
007500001222     D Errore          C                   '1'
007600001222     D PrmSocieta      S              3
007700001222     D PrmLibProj      S             10
007800001222     D PrmObjTra       S             10
007900001222     D PrmObjType      S              8
008000001222     D PrmLibTra       S             10
008100001222     D PrmEsito        S              1
008200010321     D PrmUnita        S              8
008300001222     D WrkSocieta      S
008400001222     D                                     LIKE(PrmSocieta)
008500010322     D WrkUnita        S
008600010322     D                                     LIKE(PrmUnita)
008700001227     D WrkElem         C                   20
008800010322     D WrkKey          S             39
008900001227     D Key             S
009000001227     D                                     LIKE(WrkKey)
009100001227     D                                     DIM(WrkElem)
009200001227     D LibTra          S
009300001227     D                                     LIKE(PrmLibTra)
009400001227     D                                     DIM(WrkElem)
009500001227     D i               S              5I 0
009600001222      * Parametri API QCAPCMD.
009700001222     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
009800001222     D PCmd01          S            256
009900001222     D PCmd02          S              9B 0
010000001222     D                                     INZ(%SIZE(PCmd01))
010100001222     D PCmd04          S              9B 0
010200001222     D                                     INZ(%SIZE(Qcap0100))
010300001222     D PCmd05          S              8
010400001222     D                                     INZ('CPOP0100')
010500001222     D PCmd06          S
010600001222     D                                     LIKE(PCmd01)
010700001222     D PCmd07          S              9B 0
010800001222     D                                     INZ(%SIZE(PCmd06))
010900001222     D PCmd08          S              9B 0
011000001222      * Parametro gestione errori API.
011100001222     D/COPY QSYSINC/QRPGLESRC,QUSEC
011200001222
011300001222     C     *ENTRY        PLIST
011400001222     C                   PARM                    PrmSocieta
011500001222     C                   PARM                    PrmLibProj
011600001222     C                   PARM                    PrmObjTra
011700001222     C                   PARM                    PrmObjType
011800001222     C                   PARM                    PrmLibTra
011900001222     C                   PARM                    PrmEsito
012000010321     C                   PARM                    PrmUnita
012100001222      *
012200001222     C                   EXSR      ChkParm
012300001222      *
012400001222     C                   EXSR      SetLibTra
012500001222      *
012600001222     C                   EXSR      Uscita
012700001222
012800021001     ***********************************************************************
012900021001     **
013000021001     ** Impostazione libreria *TRA*.
013100021001     **
013200021001     ***********************************************************************
013300001222     C     SetLibTra     BEGSR
013400001227      * Reperisco la libreria tra quelle già memorizzate.
013500001227     C                   EXSR      RtvLibMem
013600001222      * Librerie PROJ.
013700001222     C                   IF        %SUBST(PrmLibProj:1:5) = 'PJBAR'
013800001222     C                             OR
013900001222     C                             %SUBST(PrmLibProj:1:5) = 'PJPRV'
014000001222      * Determino la terza parte del nome della libreria.
014100001222     C                   IF        %SUBST(PrmLibProj:1:5) = 'PJBAR'
014200001222     C                   EVAL      WrkSocieta = PrmSocieta
014300001222     C                   ELSE
014400001222     C                   EVAL      WrkSocieta = 'PRV'
014500001222     C                   ENDIF
014600001222      *
014700001222     C                   SELECT
014800010322      * Ramo Bartolini ex SDI.
014900010322     C                   WHEN      PrmSocieta = '201'
015000010327     C                             AND
015100010327     C                             %PARMS > 6
015200010322     C                             AND
015300010322     C                             %SUBST(PrmUnita:1:1) = '2'
015400010327     C                   IF        WrkSocieta = '201'
015500010327     C                   EVAL      WrkSocieta = '101'
015600010327     C                   ENDIF
015700010327     C                   EVAL      PrmLibTra = 'SDITRA' + WrkSocieta
015800001222      * Ramo SDI.
015900001222     C                   WHEN      %SUBST(PrmSocieta:1:1) = '1'
016000001222     C                   EVAL      PrmLibTra = 'SDITRA' + WrkSocieta
016100010321      * Ramo Bartolini.
016200010321     C                   WHEN      %SUBST(PrmSocieta:1:1) = '2'
016300010321     C                   EVAL      PrmLibTra = 'GAITRA' + WrkSocieta
016400001222      * Ramo sconosciuto.
016500001222     C                   OTHER
016600001222     C                   EVAL      PrmEsito = Errore
016700001222     C                   EXSR      Uscita
016800001222     C                   ENDSL
016900001222      * Libreria sconosciuta.
017000001222     C                   ELSE
017100001222     C                   EVAL      PrmEsito = Errore
017200001222     C                   EXSR      Uscita
017300001222     C                   ENDIF
017400021001      * Libreria alternativa 1.
017500001222     C                   EXSR      SetLibAlt
017600001227      * Memorizzo chiave di ricerca e libreria reperita.
017700001227     C                   EXSR      MemLibTra
017800001222      *
017900001222     C                   ENDSR
018000001222
018100021001     ***********************************************************************
018200021001     **
018300021001     ** Libreria alternativa 1.
018400021001     **
018500021001     ***********************************************************************
018600001222     C     SetLibAlt     BEGSR
018700001222      * Se ho ricevuto il nome dell'oggetto, controllo l'esistenza.
018800001222     C                   IF        PrmObjTra <> *BLANK
018900001222      * Controllo l'esistenza oggetto nella libreria desunta.
019000001222     C                   EXSR      ChkObjTra
019100001222      *
019200001222     C                   SELECT
019300001222      * L'oggetto non esiste.
019400001222     C                   WHEN      Qusei = 'CPF9801'
019500021001      * Desumo la libreria alternativa 1.
019600001222     C                   EXSR      RtvAltLib
019700021001      * Controllo l'esistenza dell'oggetto nella libreria alternativa 1.
019800001222     C                   EXSR      ChkObjTra
019900021001      * Se l'oggetto non esiste nella libreria alternativa 1
020000021001      * cerco nella alternativa 2.
020100001222     C                   IF        Qusei <> *BLANK
020200021001     C                   EXSR      SetLibUni
020300001222     C                   ENDIF
020400001222      * Altri errori.
020500001222     C                   WHEN      Qusei <> *BLANK
020600001222     C                   EVAL      PrmEsito = Errore
020700001222     C                   EXSR      Uscita
020800001222      *
020900001222     C                   ENDSL
021000001222      *
021100001222     C                   ENDIF
021200001222      *
021300001222     C                   ENDSR
021400021001
021500021001     ***********************************************************************
021600021001     **
021700021001     ** Libreria alternativa 2.
021800021001     **
021900021001     ***********************************************************************
022000021001     C     SetLibUni     BEGSR
022100021001      * Desumo la libreria alternativa 2.
022200021001     C                   EXSR      RtvUniLib
022300021001      * Controllo l'esistenza dell'oggetto nella libreria alternativa 2.
022400021001     C                   EXSR      ChkObjTra
022500021001
022600021001     C                   IF        Qusei <> *BLANK
022700021001     C                   EVAL      PrmEsito = Errore
022800021001     C                   EXSR      Uscita
022900021001     C                   ENDIF
023000021001
023100021001     C                   ENDSR
023200001222
023300021001     ***********************************************************************
023400021001     **
023500021001     ** Controllo l'esistenza dell'oggetto nella libreria.
023600021001     **
023700021001     ***********************************************************************
023800001222     C     ChkObjTra     BEGSR
023900001222      * Controllo l'esistenza dell'oggetto.
024000001222     C                   EVAL      PCmd01 = 'CHKOBJ'
024100001222     C                             +
024200001222     C                             ' OBJ('
024300001222     C                             + %TRIMR(PrmLibTra) + '/'
024400001222     C                             + %TRIMR(PrmObjTra) + ')'
024500001222     C                             +
024600001222     C                             ' OBJTYPE('
024700001222     C                             + %TRIMR(PrmObjType)  + ')'
024800001222     C                   CLEAR                   Qusec
024900001222     C                   EVAL      Qusbprv = %SIZE(Qusec)
025000001222     C                   EXSR      CalQCAPCMD
025100001222      *
025200001222     C                   ENDSR
025300001222
025400021001     ***********************************************************************
025500021001     **
025600021001     ** Impostazione libreria alternativa 1.
025700021001     **
025800021001     ***********************************************************************
025900001222     C     RtvAltLib     BEGSR
026000001222      *
026100001222     C                   SELECT
026200001222      * Da GAITRA2XX e SDITRA1XX a GAITRAGRU.
026300001222     C                   WHEN      %SUBST(PrmLibTra:4:3) = 'TRA'
026400001222     C                             AND
026500001222     C                             %SUBST(PrmLibTra:7:3) <> 'PRV'
026600001222     C                   EVAL      PrmLibTra = 'GAITRAGRU'
026700001222      * Da GAITRAPRV e SDITRAPRV a GAITRAGRP.
026800001222     C                   WHEN      %SUBST(PrmLibTra:4:6) = 'TRAPRV'
026900010713     C                   EVAL      PrmLibTra = 'GAITRAGRPS'
027000001222     C                   ENDSL
027100001222      *
027200001222     C                   ENDSR
027300021001
027400021001     ***********************************************************************
027500021001     **
027600021001     ** Impostazione libreria alternativa 2.
027700021001     **
027800021001     ***********************************************************************
027900021001     C     RtvUniLib     BEGSR
028000021001      *
028100021001     C                   SELECT
028200021001     C                   WHEN      PrmLibTra = 'GAITRAGRU'
028300021001     C                   EVAL      PrmLibTra = 'UNITRAGRU'
028400021001     C                   WHEN      PrmLibTra = 'GAITRAGRPS'
028500021001     C                   EVAL      PrmLibTra = 'UNITRAGRP'
028600021001     C                   ENDSL
028700021001      *
028800021001     C                   ENDSR
028900001222
029000021001     ***********************************************************************
029100021001     **
029200021001     ** Controllo parametri ricevuti.
029300021001     **
029400021001     ***********************************************************************
029500001222     C     ChkParm       BEGSR
029600001222      *
029700001222     C                   CLEAR                   PrmLibTra
029800001222     C                   CLEAR                   PrmEsito
029900001222      *
030000001222     C                   IF        PrmSocieta = *BLANK
030100001222     C                             OR
030200001222     C                             PrmLibProj = *BLANK
030300001222     C                   EVAL      PrmEsito = Errore
030400001222     C                   EXSR      Uscita
030500001222     C                   ENDIF
030600001222      *
030700001222     C                   IF        PrmObjTra <> *BLANK
030800001222     C                             AND
030900001222     C                             PrmObjType = *BLANK
031000001222     C                   EVAL      PrmEsito = Errore
031100001222     C                   EXSR      Uscita
031200001222     C                   ENDIF
031300001222      *
031400001222     C                   ENDSR
031500001227
031600021001     ***********************************************************************
031700021001     **
031800021001     ** Reperimento libreria memorizzata.
031900021001     **
032000021001     ***********************************************************************
032100001227     C     RtvLibMem     BEGSR
032200001227      *
032300010322     C                   IF        %PARMS > 6
032400010322     C                   EVAL      WrkUnita = PrmUnita
032500010322     C                   ELSE
032600010322     C                   CLEAR                   WrkUnita
032700010322     C                   ENDIF
032800010322      *
032900001227     C                   EVAL      WrkKey
033000001227     C                             =
033100001227     C                             PrmSocieta
033200001227     C                             +
033300001227     C                             PrmLibProj
033400001227     C                             +
033500001227     C                             PrmObjTra
033600001227     C                             +
033700001227     C                             PrmObjType
033800010322     C                             +
033900010322     C                             WrkUnita
034000001227      *
034100001227     C                   EVAL      i = 1
034200001227     C                   DOU       i > WrkElem
034300001227     C                             OR
034400001227     C                             Key(i) = *BLANK
034500001227     C                   IF        Key(i) = WrkKey
034600001227     C                   EVAL      PrmLibTra = LibTra(i)
034700001227     C                   EXSR      Uscita
034800001227     C                   ELSE
034900001227     C                   EVAL      i = i + 1
035000001227     C                   ENDIF
035100001227     C                   ENDDO
035200001227      *
035300001227     C                   ENDSR
035400001227
035500021001     ***********************************************************************
035600021001     **
035700021001     ** Memorizzo libreria reperita.
035800021001     **
035900021001     ***********************************************************************
036000001227     C     MemLibTra     BEGSR
036100001227      * Se nelle schiere c'è ancora posto
036200001227      * memorizzo la chiave di ricerca e la libreria corrispondente.
036300001227     C                   IF        Key(WrkElem) = *BLANK
036400001227     C                   EVAL      i = 1
036500001227     C     *BLANK        LOOKUP    Key(i)                                 01
036600001227     C                   IF        *IN01
036700001227     C                   EVAL      Key(i) = WrkKey
036800001227     C                   EVAL      LibTra(i) = PrmLibTra
036900001227     C                   ENDIF
037000001227     C                   ENDIF
037100001227      *
037200001227     C                   ENDSR
037300001222
037400021001     ***********************************************************************
037500021001     **
037600021001     ** Esecuzione comando.
037700021001     **
037800021001     ***********************************************************************
037900001222     C     CalQCAPCMD    BEGSR
038000001222      *
038100001222     C                   CLEAR                   Qcap0100
038200001222     C                   EVAL      Qcabcsdh = '0'
038300001222     C                   EVAL      Qcapa = '0'
038400001222     C                   EVAL      Qcacmdss = '0'
038500001222     C                   EVAL      Qcaerved = X'000000000000000000'
038600001222      *
038700001222     C                   CALL      'QCAPCMD'
038800001222     C                   PARM                    PCmd01
038900001222     C                   PARM                    PCmd02
039000001222     C                   PARM                    Qcap0100
039100001222     C                   PARM                    PCmd04
039200001222     C                   PARM                    PCmd05
039300001222     C                   PARM      *BLANK        PCmd06
039400001222     C                   PARM                    PCmd07
039500001222     C                   PARM                    PCmd08
039600001222     C                   PARM                    Qusec
039700001222      *
039800001222     C                   ENDSR
039900001227
040000021001     ***********************************************************************
040100021001     **
040200021001     ** Uscita dal programma.
040300021001     **
040400021001     ***********************************************************************
040500001227     C     Uscita        BEGSR
040600001227      *
040700001227     C                   IF        PrmEsito = *BLANK
040800001227     C                   EVAL      PrmEsito = Eseguito
040900001227     C                   ENDIF
041000001227      *
041100001227     C                   RETURN
041200001227      *
041300001227     C                   ENDSR
