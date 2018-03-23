000100001020      /TITLE Controllo utente.
000200001019      *
000300001020      * Controlla l'esistenza di un utente (o di parte di esso)
000400010511      * sulla tabella 'CMU' e la Sede/Filiale di  appartenenza.
000500001020      *
000600001019      * Riceve:
000700001020      *
000800001020      * PRMUTENTE  Utente da controllare.
000900001019      *
001000010511      * PRMFILIALE Sede/Filiale di appartenenza.
001100001020      *
001200001019      * Restituisce:
001300001020      *
001400001019      * PRMESITO   Esito del controllo:
001500010511      *            '0' = Eseguito.
001600010511      *            '1' = Utente Errato.
001700010511      *            '2' = Incongruenza Utente/Filiale
001800001019      * PRMUNI     Dati dell'elemento della tabella (descrizione).
001900001019      *            Questo parametro è restituito solo se è ricevuto.
002000001019      *
002100001019
002200001019     FTnTbe01L  IF   E           K DISK
002300010511     FAzOrg01L  IF   E           K DISK
002400010511
002500010511     D Eseguito        C                   '0'
002600010511     D Errore          C                   '1'
002700010511     D Incongruenza    C                   '2'
002800001020     D PrmUtente       S             10
002900010511     D PrmFiliale      S              3  0
003000001019     D PrmEsito        S              1
003100001019     D PrmUni          S
003200001019     D                                     LIKE(TbeUni)
003300001019
003301021004     D dataiso         s               d   datfmt(*iso)
003302021004     D WDATA           s              8  0
003400010511     D DCMU          E DS
003500010511     D                                     INZ
003601021004     D fnlv55ds      E DS
003700001019     C     *ENTRY        PLIST
003800001020     C                   PARM                    PrmUtente
003900010511     C                   PARM                    PrmFiliale
004000001019     C                   PARM                    PrmEsito
004100001019     C                   PARM                    PrmUni
004200001019      *
004300001019     C                   EXSR      RcvParm
004400001019      *
004500001020     C                   EXSR      ChkUtente
004600001019      *
004700010511     C                   EXSR      ChkFiliale
004800010511      *
004900001019     C                   EXSR      Uscita
005000001019
005100010511      /TITLE Controllo utente
005200001020     C     ChkUtente     BEGSR
005300001020      * Prima provo con i primi 2 caratteri,
005400001020      * poi   provo con i primi 3 caratteri,
005500001020      * poi   provo con tutto l' utente.
005600001020     C                   EVAL      TbeKe1 = %SUBST(PrmUtente:1:2)
005700001019     C     K05Tbe01      CHAIN     TnTbe01L
005800001019     C                   IF        NOT %FOUND(TnTbe01L)
005900001020     C                   EVAL      TbeKe1 = %SUBST(PrmUtente:1:3)
006000001019     C     K05Tbe01      CHAIN     TnTbe01L
006100001019     C                   IF        NOT %FOUND(TnTbe01L)
006200001020     C                   EVAL      TbeKe1 = PrmUtente
006300001019     C     K05Tbe01      CHAIN     TnTbe01L
006400001019     C                   IF        NOT %FOUND(TnTbe01L)
006500010511     C                   EVAL      PrmEsito = Errore
006600001019     C                   ENDIF
006700001019     C                   ENDIF
006800001019     C                   ENDIF
006900001020      * Restituisco i dati dell'elemento se ho ricevuto il parametro.
007000001019     C                   IF        %FOUND(TnTbe01L)
007100001019     C                             AND
007200010511     C                             %PARMS > 3
007300001019     C                             AND
007400001019     C                             %ADDR(PrmUni) <> *NULL
007500001019     C                   EVAL      PrmUni = TbeUni
007600001019     C                   ENDIF
007700001019      *
007800001019     C                   ENDSR
007900001019
008000010511      /TITLE Controllo filiale
008100010511     C     ChkFiliale    BEGSR
008200010511      *
008300010511     C                   IF        %FOUND(TnTbe01L)
008400011105     C                             AND
008500011105     C                             PrmFiliale <> 0
008600010511
008700010511     C                   EVAL      DCMU = TbeUni
008800010511
008900010511     C                   EVAL      OrgFil = PrmFiliale
008901021004     c* reperisco terminal partenza
008902021004     c* per determinare se filiale a cui inviare è
008903021004     c* Sede 1liv. 2Liv
008904021004     C                   move      *blank        wwlpo             1
008905021004      * Reperisco data del giorno per calcolo terminal
008906021004     C                   TIME                    dataiso
008907021004     C     *iso          movel     dataiso       wdata
008908021004     c                   clear                   FNLV55DS
008909021004     c                   movel     'L'           d55tla
008910021004     c                   movel     'P'           D55TPT
008911021004     c                   movel     orgfil        d55lin
008912021004     c                   movel     wdata         D55DRF
008913021004     c                   call      'FNLV55R'
008914021004     c                   parm                    fnlv55ds
008915021004      * Ricavo il livello del P.O. solo se ho trovato i terminal
008916021004     C                   SELECT
008917021004     C                   WHEN      D55Err <> *blanks
008918021004     C                   WHEN      D55TFP = orgfil
008919021004     C                   eval      wwlpo = '1'
008920021004     C                   OTHER
008921021004     C                   eval      wwlpo = '2'
008922021004     C                   ENDSL
008923021004     c     orgfil        ifeq      046
008924021004     C                   eval      wwlpo = 'S'
008925021004     c                   endif
008926021004     C***
009000010511     C     K01Org01      CHAIN     AzOrg01l
009100010511     C                   IF        %FOUND(AzOrg01L)
009200010511
009300010511     C                   SELECT
009400010511
009500011122     ** L'utente di filiale di I livello o di I e II livello
009600011122     ** è autorizzato alle filiali di I livello.
009700011019     C                   WHEN      (§CmuUbicaz = '1'
009800011019     C                              OR
009900011019     C                              §CmuUbicaz = 'F')
010000011122     C                             AND
010500021004     C                             wwlpo = '1'
010600010511
010700011122     ** L'utente di filiale di II livello o di I e II livello
010800011122     ** è autorizzato alle filiali di II livello.
010900011019     C                   WHEN      (§CmuUbicaz = '2'
011000011019     C                              OR
011100011019     C                              §CmuUbicaz = 'F')
011200011122     C                             AND
011701021004     C                             wwlpo = '2'
011800010511
011900011122     ** L'utente di sede è autorizzato solo alla sede.
012000010511     C                   WHEN          §CmuUbicaz = 'S'
012100021004     C                             AND
012101021004     C                             wwlpo = 'S'
012200010511
012300010511     C                   WHEN          §CmuUbicaz = ' '
012400010511
012500010511     C                   OTHER
012600010511     C                   EVAL      PrmEsito = Incongruenza
012700010511     C                   ENDSL
012800011122
012900010511     C                   ENDIF
013000011122
013100010511     C                   ENDIF
013200010511      *
013300010511     C                   ENDSR
013400010511
013500001019      /TITLE Ricezione e controllo parametri.
013600001019     C     RcvParm       BEGSR
013700001019      *
013800001019     C                   CLEAR                   PrmEsito
013900001019      *
014000010511     C                   IF        %PARMS > 3
014100001019     C                             AND
014200001019     C                             %ADDR(PrmUni) <> *NULL
014300001019     C                   CLEAR                   PrmUni
014400001019     C                   ENDIF
014500001020      * L'utente da controllare è obbligatorio.
014600010511     C                   IF           PrmUtente  = *BLANK
014700010511     C                   EVAL      PrmEsito = Errore
014800001019     C                   EXSR      Uscita
014900001019     C                   ENDIF
015000001019      *
015100001019     C                   ENDSR
015200001019
015300001019      /TITLE Operazioni finali.
015400001019     C     Uscita        BEGSR
015500001019      *
015600001019     C                   IF        PrmEsito = *BLANK
015700010511     C                   EVAL      PrmEsito = Eseguito
015800001019     C                   ENDIF
015900001019      *
016000001019     C                   RETURN
016100001019      *
016200001019     C                   ENDSR
016300001019
016400001019      /TITLE Operazioni iniziali.
016500001019     C     *INZSR        BEGSR
016600001019      *
016700001019     C     K05Tbe01      KLIST
016800001019     C                   KFLD                    TbeCod
016900001019     C                   KFLD                    TbeKe1
017000001019     C                   KFLD                    TbeKe2
017100001019     C                   KFLD                    TbeLin
017200001019     C                   KFLD                    TbeSif
017300001019      *
017400010511     C     K01Org01      KLIST
017500010511     C                   KFLD                    OrgFil
017600010511      *
017700001019     C                   EVAL      TbeCod = 'CMU'
017800001019      *
017900001019     C                   ENDSR
