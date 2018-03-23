000100020103     H DECEDIT('0,') DATEDIT(*DMY.)
000200020419
000300020419      *--------------------------------------------------------------*
000400011214      *      UTILITY DI CONTROLLO PER PROFILI UTENTE                 *
000500011217      *--------------------------------------------------------------*
000600020419
000700020419      *--------------------------------------------------------------*
000800020419      *      FORZATO IL CAPOCONTO CLIENTI 151  (Wcpc)                *
000900020419      *--------------------------------------------------------------*
001000020419
001100020314     FAzUte01L  IF   E           K DISK    usropn
001200020920     FAzUte02L  IF   E           K DISK    usropn  rename(AzUte000:AzUte02)
001300020920     FUtent00f  IF   E           K DISK    usropn
001400020920     FUtent02l  IF   E           K DISK    usropn  rename(Utent000:Utent02)
001500020409     FAzOrg01L  IF   E           K DISK
001600020409      *
001700020419      * Definisco Parut00f come file esterno. Quindi per la chain
001800020419      * devo definire la chiave come DS
001900020409     FPARUT00F  IF   F  163     3AIDISK    KEYLOC(11)
002000020419      *--------------
002100020409     D KRut            DS
002200020409     D  ParKut                 1      1P 0
002300020409     D  ParTip                 2      3P 0
002400971210      *--------------------------------------------------------------*
002500011217      * Ds per reperimento terminal di linea
002600011214     D FnLv55Ds      E DS
002700011217      *--------
002800011217      * Ds di riferimento al file esterno AzUte00F
002900020405     D AzuteDs       E DS                  ExtName(AzUte00F)
003000020319      *--------
003100020319      * Ds per dati organigramma
003200020405     D DDatiUte      E DS
003300020219      *--------
003400020219      * Descrizione dell'organigramma
003500020314     D Og143         E DS
003600020405     D CNCR80        E DS                  prefix(§§)
003700020402     D TIBS34DS      E DS
003800020104      *---------------------
003900020104      * Se la data area non esiste in *LIBL viene creata dal pgm in QTEMP
004000020418     D §AZUTE         UDS           999    DTAARA(§AZUTE)
004100020104      *---------------------
004200020104      * Se la data area non esiste in *LIBL viene creata dal pgm in QTEMP
004300020418     D §DatiUte       UDS           999    DTAARA(§DatiUte)
004400020314      *---------------------
004500020419      * ricavo profilo del JOB
004600020408     DINFDS           SDS
004700020419     D USER                  254    263
004800020408      *---------------------
004900020409     D w003A           s              3
005000020409     D $Pou            s              3  0
005100020409     D $noAzUte        s              1
005200020409     D dataiso         s               d   datfmt(*iso)
005300020409     D WDATA           s              8  0
005400020419     d Wute            s                   like(I34ute)
005500020419     d Wcpc            s                   like(DutKci)  INZ(0151)
005600020402      *--------------------------------------------------------------*
005700020424     D Msg             S             78    DIM(04) CTDATA PERRCD(1)
005800020409      *--------------------------------------------------------------*
005900020417     IPARUT00F  AA  01
006000020409     I                                 14  163  CA150
006100020417     I                                 14   33  RagSoc
006200971212      *--------------------------------------------------------------*
006300011217      *  MAIN
006400971210      *--------------------------------------------------------------*
006500020402      *
006600020402      * Ricevo parametri in entrata
006700020402     C     *ENTRY        PLIST
006800020404     C                   PARM                    Tibs34Ds
006900020405      * Pulizia dei campi di output
007000020409     C                   Exsr      PulDati
007100020405      *
007200020405      * Se non richiesta fine elaborazione
007300020405    1C                   IF        I34Tla <> 'C'
007400020402      * Tipo lancio
007500020403      * Se non impostato correttamente Tipo lancio o se ho richiesto
007600020403      * output in data area si chiude sempre con LR
007700020402      *
007800020405    2C                   IF        I34Tla <> ' ' and  I34Tla <> 'L'
007900020409     C                             or I34Dta = ' '
008000020402     C                   eval      I34Tla ='L'
008100020507   2-C                   ENDIF
008200020409      *
008300020409      * Se fra i dati di Input non ho passato nè il Profilo, nè il P.O.,
008400020409      * nè il Cod.Utente, imposto il Profilo uguale a quello del job
008500020409    2C                   IF        I34Ute = *blanks
008600020419     C                             and I34Pou = *zeros and I34Cou = *zeros
008700020419     C                   eval      Wute = User
008800020419     c                   ELSE
008900020419     C                   eval      Wute = I34ute
009000020409   2-C                   ENDIF
009100020423      *--------------------------------------------------------------------
009200020611      * SE RICHIAMATO CON DTAARA LA Rag.Soc.Utente (RSUT) DEVE ESSERE SEMPRE
009300020611      *  PIENA: I CHIAMANTI TESTANO QUESTO CAMPO PER VERIFICARE SE LE
009400020507      *  DTAARA SONO GIÀ STATE VALORIZZATE
009500020423      *--------------------------------------------------------------------
009600020417      * Ragione sociale Utente
009700020417      * Se richiesto output in modalità data area
009800020417    2C                   IF        I34Dta = ' '
009900020417     C                   Exsr      RepRsUt
010000020507   2-C                   ENDIF
010100020423      *--------------------------------------------------------
010200020405      *
010300020417      * RECUPERO DATI UTENTE
010400020417      * Distinguo i due casi:Se ho AzUte lo leggo,altrimenti uso Utent
010500020409   2aC                   IF        %OPEN(AZUTE01L)
010600020405     C                   Exsr      GesAzUte
010700020409      *
010800020507   2eC                   ELSE
010900020409     C                   Exsr      GesUtent
011000020417   2-C                   ENDIF
011100020424      *
011200020507      * RECUPERO DATI DA AZORG
011300020507      *  Se non esiste AzUte oppure se esite AzUte e non sono in errore
011400020507   2aC                   IF        NOT %OPEN(AZUTE01L) or
011500020507     C                             (O34Err<>'E' and DutErr<>'E')
011600020402     C                   Exsr      GesOrg
011700020424   2-C                   ENDIF
011800020403      *
011900020922      * Se output DS ricavo il terminal partenza
012000020922    2C                   IF        I34Dta = 'S' and O34err = *blanks and
012100020923     C                             O34pou <> *zeros  and O34pou <> 046
012200020922     C                   exsr      RepTp
012300020922    2C                   ENDIF
012400020922      *
012500020922      * Se output DATAAREA
012600020922    2C                   IF        I34Dta = *blanks
012700020922      *   Imposto SIMFEL e i TERMINAL P/A
012800020402     C                   Exsr      GesTpa
012900020611      *   Trucco RSUT
013000020625     C                   movel     DutPou        w003a
013100020625     C                   eval      RSUT = %trim(RsUtOld) +
013200020625     c                                    ' (' + W003A + ')'
013300020922      *   Aggiorno area dati Dati Utente
013400020405     C     *lock         in        §DatiUte
013500020405     C                   movel     DDatiUte      §DatiUte
013600020405     C                   out       §DatiUte
013700020405    2C                   ENDIF
013800020404      *
013900020405    1C                   ENDIF
014000020403      *
014100020403      * Operazioni finali
014200020404     C                   Exsr      EndPgm
014300020319      *----------------------------------------------------------*
014400020319      * Dati Utente
014500020319      *----------------------------------------------------------*
014600020405     C     GesAzUte      BegSr
014700020319      *
014800020419  1a C                   IF        WUte  <> *blanks
014900020419     C     Wute          chain     AzUte01L                           21
015000020409     C                   ELSE
015100020409     C     K02Ute        chain     AzUte02l                           21
015200020409  1- C                   ENDIF
015300020409      *
015400020419      * Se ho trovato il profilo ............
015500020405  1a C                   IF        *in21 = *off
015600020409     C                   clear                   $NoAzUte
015700020419      *     imposto il P.O di gestione per agganciare AZORG
015800020403     C                   move      UteFil        $Pou
015900020405      * Segnalo che non ho trovato l'utente
016000020419  1e C                   ELSE
016100020409     C                   move      '1'           $NoAzUte
016200020405  1- C                   ENDIF
016300020409      *
016400020517      * Aggiorno dati in DS o DTAARA in base a:
016500020517      *    - tipo output
016600020517      *    - trovato profilo scaduto nel file
016700020517      *    - trovato profilo valido nel file
016800020517      *    - non trovato profilo valido nel file
016900020409  1  C                   SELECT
017000020409      *
017100020517      *--OUT: dtaara  PROFILO: trovato, scaduto
017200020409     C                   WHEN      $NoAzUte = ' ' and  I34Dta = *blanks
017300020517     C                             and  Wdata > UteDsc
017400020424     C                   eval      DutErr = 'E'
017500020424     C                   eval      DUtMsg = Msg(3)
017600020517      *    Aggiorno area dati del file utenti
017700020517     C     *lock         in        §AZUTE
017800020517     C                   movel     AzUteDS       §AZUTE
017900020517     C                   out       §AZUTE
018000020517      *
018100020517      *--OUT: dtaara  PROFILO: trovato, non scaduto
018200020517     C                   WHEN      $NoAzUte = ' ' and  I34Dta = *blanks
018300020419     C                   z-add     Wcpc          DUtKci
018400020405     C                   move      UteFil        DUtPou
018500020405     C                   move      UteCdu        DutCou
018600020426     C                   eval      DutUte = UteUte
018700020517      *    Aggiorno area dati del file utenti
018800020405     C     *lock         in        §AZUTE
018900020319     C                   movel     AzUteDS       §AZUTE
019000020319     C                   out       §AZUTE
019100020517      *--------------------------------------------------------
019200020517      *   ATTENZIONE: "TRUCCO" per cambio S.I.
019300020517      *      Se pgm riceve I34pou impostato e I34cou non impostato
019400020517      *      e il p.o. utente è 046 FORZO I34pou come p.o. utente
019500020527     C                   If        I34pou <> *zeros  and  I34cou = *zeros
019600020517     c                             and  UTEfil = 046
019700020517     C                   move      I34pou        $Pou
019800020517     C                   move      I34pou        DUtPou
019900020517     c                   Endif
020000020517      *--------------------------------------------------------
020100020409      *
020200020517      *--OUT: dtaara  PROFILO: non trovato
020300020409     C                   WHEN      $NoAzUte = '1' and  I34Dta = *blanks
020400020517     C                   eval      DutErr = 'E'
020500020517      *       Imposto msg in base al tipo richiamo
020600020517  1a C                   If        I34Ute <> *blanks or Wute <> *blanks
020700020409     C                   eval      DUtMsg = Msg(1)
020800020517     C                   Else
020900020409     C                   eval      DUtMsg = Msg(2)
021000020517     C                   Endif
021100020409      *
021200020517      *--OUT: ds      PROFILO: trovato, scaduto
021300020409     C                   WHEN      $NoAzUte = ' ' and I34Dta <> *blanks
021400020517     C                             and  Wdata > UteDsc
021500020424     C                   eval      O34Err = 'E'
021600020424     C                   eval      O34Msg = Msg(3)
021700020517      *
021800020517      *--OUT: ds      PROFILO: trovato, non scaduto
021900020517     C                   WHEN      $NoAzUte = ' ' and I34Dta <> *blanks
022000020409     C                   eval      O34Ute = UteUte
022100020409     C                   move      UteFil        O34Pou
022200020409     C                   move      UteCdu        O34Cou
022300020402     C                   movel     AzUteDs       O34Rut
022400020409      *
022500020517      *--OUT: ds      PROFILO: non trovato
022600020405     C                   OTHER
022700020403     C                   eval      O34Err = 'E'
022800020517      *       Imposto msg in base al tipo richiamo
022900020419  1a C                   if        I34Ute <> *blanks or Wute <> *blanks
023000020403     C                   eval      O34Msg = Msg(1)
023100020409     C                   else
023200020409     C                   eval      O34Msg = Msg(2)
023300020409  2- C                   endif
023400020409      *
023500020409  1- C                   ENDSL
023600020403      *
023700020319     C                   EndSr
023800020409      *----------------------------------------------------------*
023900020409      * Dati Utente
024000020409      *----------------------------------------------------------*
024100020409     C     GesUtent      BegSr
024200020409      *
024300020419     C                   IF        Wute <> *blanks
024400020419     C     Wute          chain     Utent00f                           21
024500020409     C                   ELSE
024600020409     C     I34Cou        chain     Utent02l                           21
024700020409     C                   ENDIF
024800020409      *
024900020409      *  Se ho trovato il profilo ............
025000020409  1a C                   IF        *in21 = *off
025100020419     C                   eval      WUte = UtnNam
025200020509      *
025300020509      * In base al tipo di output aggiorno dati per Ds o Dataara
025400020509  1  C                   SELECT
025500020509      *
025600020509      *--Profilo trovato e out = DS
025700020509  1  C                   WHEN      I34Dta <>*blanks
025800020509     C                   eval      O34Ute = UtnNam
025900020509     C                   move      UtnCod        O34Cou
026000020509      *
026100020509      *--Profilo trovato e out = dataara
026200020509  1  C                   WHEN      I34Dta = *blanks
026300020509     C                   eval      DutUte = UtnNam
026400020509     C                   move      UtnCod        DutCou
026500020509      *
026600020509  1- C                   ENDSL
026700020509      *
026800020409  1- C                   ENDIF
026900020507      *
027000020507      * Recupero il P.O. di gestione
027100020507      *
027200020507      *  Se il profilo inizia per REMxxx il P.O corrisponde a xxx
027300020507   1aC                   IF        %subst(Wute:1:3) = 'REM'  and
027400020507     C                             %subst(Wute:4:3) > '000'  and
027500020507     C                             %subst(Wute:4:1) >= '0'   and
027600020507     C                             %subst(Wute:5:1) >= '0'   and
027700020507     C                             %subst(Wute:6:1) >= '0'
027800020507     C                   eval      W003A = %subst(Wute:4:3)
027900020507     C                   movel     W003A         $PoU
028000020507   1eC                   ELSE
028100020507      *
028200020507      *  Il P.O in questo caso è Simfel: aggancio parut
028300020517     c                   exsr      Rep§SIMFEL
028400020517      *
028500020521      * Se Simfel è a zero significa che siamo in sede.
028600020521      *   In questo caso imposto 046, ma questo caso non si verifica dato
028700020521      *   che in sede deve esistere AZUTE00F
028800020517   2aC                   IF        §§Simfel <> *zeros
028900020507     C                   move      §§Simfel      $PoU
029000020517   2eC                   ELSE
029100020507     C                   z-add     046           $PoU
029200020517   2-C                   ENDIF
029300020517      *
029400020507   1-C                   ENDIF
029500020409      *
029600020409     C                   EndSr
029700971231      *----------------------------------------------------------*
029800011214      * Aggancio AzOrg
029900971231      *----------------------------------------------------------*
030000011217     C     GesOrg        BEGSR
030100020104      *
030200020403     C     $Pou          chain     AzOrg01l
030300020402      *
030400020507    1C                   SELECT
030500020507      *
030600020507      *--Aggiorno dati per DATA AREA e Organigramma trovato
030700020507    1C                   WHEN      I34dta = *blanks  and
030800020507     C                             %FOUND(AZORG01L)
030900020419     C                   z-add     wcpc          DUtKci
031000020423     C                   move      OrgFil        DUtPou
031100020405     C                   eval      DUtDPo = OrgDes
031200020405     C                   eval      DUtDis = OrgFl3
031300020410     C                   move      OrgCar        DutAre
031400020417     C                   move      OrgFel        DutFel
031500020417     C                   eval      Og143  = OrgDe3
031600020410     C                   eval      DUtDe3 = Og143
031700020410     C                   eval      DUtNtw = §ogntw
031800020417     C                   move      §OgAs         DutAsI
031900020507      *
032000020507      *--Aggiorno dati per DATA AREA e Organigramma NON trovato
032100020507    1C                   WHEN      I34dta = *blanks  and
032200020507     C                             NOT %FOUND(AZORG01L)
032300020507     C                   move      'E'           DutErr
032400020507     C                   movel     Msg(4)        DutMsg
032500020507      *
032600020507      *--Aggiorno dati per DS e Organigramma trovato
032700020507    1C                   WHEN      I34dta = 'S' and  %FOUND(AZORG01L)
032800020507     C                   move      $Pou          O34Pou
032900020404     C                   eval      O34DPo = OrgDes
033000020402     C                   eval      O34Dis = OrgFl3
033100020402     C                   eval      O34Car = OrgCar
033200020417     C                   move      OrgFel        O34Fel
033300020417     C                   eval      O34De3 = OrgDe3
033400020417     C                   eval      Og143  = OrgDe3
033500020402     C                   eval      O34Ntw = §OgNtw
033600020417     C                   move      §OgAs         O34AsI
033700020507      *
033800020507      *--Aggiorno dati per DS e Organigramma NON trovato
033900020507    1C                   WHEN      I34dta = 'S' and NOT %FOUND(AZORG01L)
034000020507     C                   move      $Pou          O34Pou
034100020424     C                   move      'E'           O34Err
034200020424     C                   movel     Msg(4)        O34Msg
034300020507      *
034400020507    1C                   ENDSL
034500020507      *
034600020507     C                   ENDSR
034700011217      *----------------------------------------------------------*
034800011217      * Terminal Partenza/Arrivo
034900011217      *----------------------------------------------------------*
035000011217     C     GesTPA        BegSr
035100011220      *
035200020521      *  Recupero Simfel da PARUT
035300020521     c                   exsr      Rep§SIMFEL
035400020521      *
035500020527      *  Se §§SIMFEL = 0 mi trovo sul AS di sede nel S.I. di sede
035600020527      *  imposto quindi: SIMFEL = 0;
035700020527    1C                   IF        §§SIMFEL = *zeros
035800020521     C                   eval      SimFel = §§SIMFEL
035900020527      *     Se P.O. utente non sede ricavo i terminal e il tipo p.o.
036000020527      *     Se P.O. utente è sede imposto il tipo p.o.
036100020527    2c                   IF        $POU <> 046
036200020527     C                   exsr      RepTpTa
036300020527     C                   move      d55Tfa        DutTfa
036400020527     C                   move      d55Tfp        DutTfp
036500020527     C                   move      Wdata         DutDte
036600020527   x2C                   ELSE
036700020527     C                   eval      DutLPo = 'S'
036800020527    2C                   ENDIF
036900020521      *
037000020521      *    Se §§SIMFEL <> 0 mi trovo sul AS di sede nel S.I. di P.O.
037100020527      *    o sul AS di un p.o. quindi imposto com SIMFEL il terminal
037200020527      *    partenza del p.o. UTENTE
037300020527   x1C                   ELSE
037400011220      *
037500020527      * Ricavo entrambi i terminal, Partenza ed arrivo
037600020527     C                   EXSR      RepTpTa
037700011217      *
037800020405     C                   move      d55Tfa        DutTfa
037900020405     C                   move      d55Tfp        DutTfp
038000020405     C                   move      d55Tfp        Simfel
038100020405     C                   move      Wdata         DutDte
038200020507      *
038300020527    1C                   ENDIF
038400020403      *
038500011217     C                   EndSr
038600020403      *----------------------------------------------------------*
038700020403      * Pulizia campi output
038800020403      *----------------------------------------------------------*
038900020409     C     PulDati       BEGSR
039000020409      *
039100020409      * Se sono in modalità DataAra pulisco le dataara
039200020409    2C                   IF        I34Dta = *blanks
039300020403     C                   clear                   DDatiUte
039400020409     C     *lock         in        §DatiUte
039500020409     C                   movel     DDatiUte      §DatiUte
039600020409     C                   out       §DatiUte
039700020405     C                   clear                   AzUteDs
039800020409     C     *lock         in        §AZUTE
039900020409     C                   movel     AzUteDS       §AZUTE
040000020409     C                   out       §AZUTE
040100020409     C                   ENDIF
040200020403      *
040300020409      * Se sono in modalità Ds pulisco la DS
040400020409    2C                   IF        I34Dta = 'S'
040500020409     C                   clear                   AzUteds
040600020409     C                   movel     AzUteDs       O34Rut
040700020405     C                   clear                   O34UTE
040800020405     C                   clear                   O34POU
040900020405     C                   clear                   O34COU
041000020405     C                   clear                   O34DPO
041100020405     C                   clear                   O34DIS
041200020405     C                   clear                   O34CAR
041300020405     C                   clear                   O34FEL
041400020417     C                   clear                   O34ASI
041500020405     C                   clear                   O34NTW
041600020405     C                   clear                   O34DE3
041700020922     C                   clear                   O34TFP
041800020922     C                   clear                   O34FLO
041900020405     C                   clear                   O34ERR
042000020405     C                   clear                   O34MSG
042100020409     C                   ENDIF
042200020403      *
042300020403     C                   EndSr
042400020417      *----------------------------------------------------------*
042500020417      * Reperisco Ragione sociale Utente
042600020417      *----------------------------------------------------------*
042700020417     C     RepRsUt       BEGSR
042800020417      *
042900020417     C                   z-add     1             ParKut
043000020417     C                   z-add     10            ParTip
043100020417     C     Krut          chain     Parut00f
043200020417   5aC                   IF        %FOUND(PARUT00F)
043300020611     C                   movel     RagSoc        RsUtOld
043400020611     C                   movel     RagSoc        RsUt
043500020417     C                   ENDIF
043600020417      *
043700020611      * Dopo aver determinato il p.o. utente Rsut viene truccato
043800020611      * aggiungendo il p.o..
043900020611      *
044000020417     C                   EndSr
044100020517      *----------------------------------------------------------*
044200020517      * Reperisco SIMFEL dal file PARUT
044300020517      *----------------------------------------------------------*
044400020517     C     Rep§SIMFEL    BEGSR
044500020517      *
044600020517     C                   clear                   Ca150
044700020517     C                   z-add     1             ParKut
044800020517     C                   z-add     80            ParTip
044900020517     C     Krut          chain     Parut00f
045000020517   2aC                   IF        %FOUND(PARUT00F)
045100020517     C                   movel     Ca150         CnCr80
045200020517     C                   ELSE
045300020517     C                   clear                   Cncr80
045400020517     c                   ENDIF
045500020517      *
045600020517     C                   EndSr
045700020527      *----------------------------------------------------------*
045800020922      * Reperisco Terminal Par/Arr e livello p.o.
045900020527      *----------------------------------------------------------*
046000020527     C     RepTpTa       BEGSR
046100020527      *
046200020527     C                   clear                   Fnlv55Ds
046300020527     c                   move      'L'           d55tla
046400020527     C                   move      $Pou          d55lin
046500020527     C                   move      Wdata         d55drf
046600020527     C                   call      'FNLV55R'
046700020527     C                   parm                    FNLV55DS
046800020527      *
046900020527      * Ricavo il livello del P.O. solo se ho trovato i terminal
047000020922     C                   SELECT
047100020922     C                   WHEN      D55Err <> *blanks
047200020922     C                   WHEN      D55TFP = $POU
047300020527     C                   eval      DutLPo = '1'
047400020922     C                   OTHER
047500020527     C                   eval      DutLPo = '2'
047600020922     C                   ENDSL
047700020527      *
047800020527     C                   EndSr
047900020922      *----------------------------------------------------------*
048000020922      * Reperisco Terminal Partenza
048100020922      *----------------------------------------------------------*
048200020922     C     RepTp         BEGSR
048300020922      *
048400020922     C                   clear                   Fnlv55Ds
048500020922     c                   move      'L'           d55tla
048600020922     c                   move      'P'           d55tpt
048700020922     C                   move      O34POU        d55lin
048800020922     C                   move      Wdata         d55drf
048900020922     C                   call      'FNLV55R'
049000020922     C                   parm                    FNLV55DS
049100020922      *
049200020923     C                   IF        D55Err = *blanks
049300020922     C                   move      d55Tfp        O34Tfp
049400020923     C                   ENDIF
049500020922      *
049600020922     C                   EndSr
049700020403      *----------------------------------------------------------*
049800020405      * Operazione di fine Pgm
049900020403      *----------------------------------------------------------*
050000020404     C     EndPgm        BEGSR
050100020507      *
050200021014      * Se richiamato in modalità DS cancello le dataara se vuote e non
050300021014      *  in errore
050400021014    2C                   IF        I34Dta = 'S'
050500021014    2C                   call      'TIBS34C'
050600021014    2C                   ENDIF
050700021014      *
050800020404      * Disalloco le aree dati
050900020507     C                   UnLock(e) §AzUte
051000020507     C                   UnLock(e) §DatiUte
051100020404      *
051200020403     C     I34Tla        ifne      ' '
051300020405     C                   eval      *inLR = *on
051400020405     C                   else
051500020405     C                   eval      *inRT = *on
051600020403     C                   endif
051700020403      *
051800020403     C                   EndSr
051900020403      *--------------------------------------------------------------------
052000020403      *  *inzsr Routine iniziale
052100020403      *--------------------------------------------------------------------
052200020402     C     *inzsr        BEGSR
052300020405      *
052400020920      * Richiamo trul25c per verificare se esiste AZUTE,se esiste lo apro.
052500020920      *  Se non esiste apro UTENT
052600020402     c                   call      'TRUL25C'
052700020402     c                   parm      '2'           §TipoC            1
052800020402     c                   parm      'AZUTE00F  '  §File            10
052900020402     c                   parm      '*LIBL     '  §Libr            10
053000020507     c                   parm      '*FIRST    '  §Mbr             10
053100020405     c                   parm      *blanks       §Text            10
053200020402     c                   parm      *blanks       §FrcRatio         1
053300020402     c                   parm      *blanks       §OvrScp           1
053400020402     c                   parm      *zeros        §Esito            2
053500020402      *
053600020403     C     §Esito        ifeq      '00'
053700020402     C                   open      AzUte01L
053800020403     C                   open      AzUte02L
053900020920     C                   else
054000020920     C                   open      UTENT00F
054100020920     C                   open      UTENT02L
054200020920     C                   endif
054300020424      *
054400020424      * Reperisco data del giorno per calcolo terminal
054500020424     C                   TIME                    dataiso
054600020424     C     *iso          movel     dataiso       wdata
054700020402      *
054800020402      * Chiavi di lettura
054900020402     C     K02Ute        KLIST
055000020402     C                   KFLD                    I34Pou
055100020402     C                   KFLD                    I34Cou
055200020507      *
055300020403     C                   Endsr
055400020405      *----------------------------------------------------------*
055500020402**
055600020709TIBS34R-Profilo utente non codificato
055700020709TIBS34R-Codice utente non codificato
055800020709TIBS34R-Profilo utente scaduto
055900020709TIBS34R-P.O. utente non presente in organigramma
