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
001300180228     F*Utent00f  IF   E           K DISK    usropn
001400180228     F*Utent02l  IF   E           K DISK    usropn  rename(Utent000:Utent02)
001500020409     FAzOrg01L  IF   E           K DISK
001600020409      *
001700020419      * Definisco Parut00f come file esterno. Quindi per la chain
001800020419      * devo definire la chiave come DS
001900180228     FPARUT00F  IF   F  163     3AIDISK    KEYLOC(11)
002000020419      *--------------
002100180228     D KRut            DS
002200180228     D  ParKut                 1      1P 0
002300180228     D  ParTip                 2      3P 0
002400971210      *--------------------------------------------------------------*
002500011217      * Ds per reperimento terminal di linea
002600011214     D FnLv55Ds      E DS
002601180226      *--------------------------------------------------------------*
002602180226      * Ds per reperimento dati fissi e data blocco fatturazione
002603180228     D tibs34ds1     E DS
002700011217      *--------
002800011217      * Ds di riferimento al file esterno AzUte00F
002900020405     D AzuteDs       E DS                  ExtName(AzUte00F)
003000020319      *--------
003100020319      * Ds per dati organigramma
003200020405     D DDatiUte      E DS
003300020219      *--------
003400020219      * Descrizione dell'organigramma
003500020314     D Og143         E DS
003600180228     D CNCR80        E DS                  prefix(§§)
003700020402     D TIBS34DS      E DS
003701180228     D Ddutxxx       E DS
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
005500180223     d*Wcpc            s                   like(DutKci)  INZ(0151)
005600020402      *--------------------------------------------------------------*
005700020424     D Msg             S             78    DIM(04) CTDATA PERRCD(1)
005800020409      *--------------------------------------------------------------*
005900180228     IPARUT00F  AA  01
006000180228     I                                 14  163  CA150
006100180228     I                                 14   33  RagSoc
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
007400020409      *
007500020409      * Se fra i dati di Input non ho passato nè il Profilo, nè il P.O.,
007600020409      * nè il Cod.Utente, imposto il Profilo uguale a quello del job
007700020409    2C                   IF        I34Ute = *blanks
007800020419     C                             and I34Pou = *zeros and I34Cou = *zeros
007900020419     C                   eval      Wute = User
007901180226     C* Richiamo pgm per recuperare alcuni dati
007902180228     C                   CLEAR                   tibs34ds1
007903180228     C                   call      'TIBS34R1'
007904180228     C                   parm                    tibs34ds1
007905180226     C                   parm                    AzuteDs
008000020419     c                   ELSE
008100020419     C                   eval      Wute = I34ute
008101180226     C* Richiamo pgm per recuperare alcuni dati
008102180226     c                   clear                   AzuteDs
008104180228     C                   CLEAR                   tibs34ds1
008105180228     C                   call      'TIBS34R1'
008106180228     C                   parm                    TIBS34ds1
008200020409   2-C                   ENDIF
008201180226
008202180226     c                   eval      dutkci = okci
008203180226     c                   eval      rsutold= oraguts
008204180226     c                   eval      rsut   = oraguts
008205180228     c                   clear                   ddutxxx
008207180228     c                   eval      §DUTBLKF = O341BLKF
008208180228     C                   eval      dutxxx   = ddutxxx
008300020423      *--------------------------------------------------------------------
008400021129      *  LA Rag.Soc.Utente (RSUT) DEVE ESSERE SEMPRE
008500020611      *  PIENA: I CHIAMANTI TESTANO QUESTO CAMPO PER VERIFICARE SE LE
008600020507      *  DTAARA SONO GIÀ STATE VALORIZZATE
008700020423      *--------------------------------------------------------------------
008800020417      * Ragione sociale Utente
008900180223     C***                Exsr      RepRsUt
009000020423      *--------------------------------------------------------
009100020405      *
009200020417      * RECUPERO DATI UTENTE
009300020417      * Distinguo i due casi:Se ho AzUte lo leggo,altrimenti uso Utent
009400050405   2aC                   IF        %OPEN(AZUTE01L) and wute<>'QPGMR     '
009500020405     C                   Exsr      GesAzUte
009600020409      *
009700020507   2eC                   ELSE
009800180228     C                   Exsr      GesUtent
009900020417   2-C                   ENDIF
010000020424      *
010100020507      * RECUPERO DATI DA AZORG
010200020507      *  Se non esiste AzUte oppure se esite AzUte e non sono in errore
010300021129   2aC                   IF        NOT %OPEN(AZUTE01L) or DutErr <> 'E'
010400020402     C                   Exsr      GesOrg
010500020424   2-C                   ENDIF
010600021129      *
010700020922      *   Imposto SIMFEL e i TERMINAL P/A
010800020402     C                   Exsr      GesTpa
010900020611      *   Trucco RSUT
011000020625     C                   movel     DutPou        w003a
011100020625     C                   eval      RSUT = %trim(RsUtOld) +
011200020625     c                                    ' (' + W003A + ')'
011300020922      *   Aggiorno area dati Dati Utente
011400020405     C     *lock         in        §DatiUte
011500020405     C                   movel     DDatiUte      §DatiUte
011600020405     C                   out       §DatiUte
011700020404      *
011800020405    1C                   ENDIF
011900020403      *
012000020403      * Operazioni finali
012100020404     C                   Exsr      EndPgm
012200020319      *----------------------------------------------------------*
012300020319      * Dati Utente
012400020319      *----------------------------------------------------------*
012500020405     C     GesAzUte      BegSr
012600020319      *
012601180228     c* UTEFIL>0 se ricevuto record di azute da tibs34r1
012602180226     c                   if        utefil>0
012603180226     c                   setoff                                       21
012605180226     c                   else
012700020419  1a C                   IF        WUte  <> *blanks
012800020419     C     Wute          chain     AzUte01L                           21
012900020409     C                   ELSE
013000020409     C     K02Ute        chain     AzUte02l                           21
013100020409  1- C                   ENDIF
013101180226     c                   endif
013200020409      *
013300020419      * Se ho trovato il profilo ............
013400020405  1a C                   IF        *in21 = *off
013500020409     C                   clear                   $NoAzUte
013600020419      *     imposto il P.O di gestione per agganciare AZORG
013700020403     C                   move      UteFil        $Pou
013800020405      * Segnalo che non ho trovato l'utente
013900020419  1e C                   ELSE
014000020409     C                   move      '1'           $NoAzUte
014100020405  1- C                   ENDIF
014200020409      *
014300021129      * Aggiorno dati in  DTAARA in base a:
014400020517      *    - trovato profilo scaduto nel file
014500020517      *    - trovato profilo valido nel file
014600020517      *    - non trovato profilo valido nel file
014700020409  1  C                   SELECT
014800020409      *
014900020517      *--OUT: dtaara  PROFILO: trovato, scaduto
015000021129     C                   WHEN      $NoAzUte = ' '
015100020517     C                             and  Wdata > UteDsc
015200020424     C                   eval      DutErr = 'E'
015300020424     C                   eval      DUtMsg = Msg(3)
015400020517      *    Aggiorno area dati del file utenti
015500020517     C     *lock         in        §AZUTE
015600020517     C                   movel     AzUteDS       §AZUTE
015700020517     C                   out       §AZUTE
015800020517      *
015900020517      *--OUT: dtaara  PROFILO: trovato, non scaduto
016000021129     C                   WHEN      $NoAzUte = ' '
016100180223     C**                 z-add     Wcpc          DUtKci
016200020405     C                   move      UteFil        DUtPou
016300020405     C                   move      UteCdu        DutCou
016400020426     C                   eval      DutUte = UteUte
016500020517      *    Aggiorno area dati del file utenti
016600020405     C     *lock         in        §AZUTE
016700020319     C                   movel     AzUteDS       §AZUTE
016800020319     C                   out       §AZUTE
016900020517      *--------------------------------------------------------
017000020517      *   ATTENZIONE: "TRUCCO" per cambio S.I.
017100020517      *      Se pgm riceve I34pou impostato e I34cou non impostato
017200020517      *      e il p.o. utente è 046 FORZO I34pou come p.o. utente
017300020527     C                   If        I34pou <> *zeros  and  I34cou = *zeros
017400020517     c                             and  UTEfil = 046
017500020517     C                   move      I34pou        $Pou
017600020517     C                   move      I34pou        DUtPou
017700020517     c                   Endif
017800020517      *--------------------------------------------------------
017900020409      *
018000020517      *--OUT: dtaara  PROFILO: non trovato
018100021129     C                   WHEN      $NoAzUte = '1'
018200020517     C                   eval      DutErr = 'E'
018300020517      *       Imposto msg in base al tipo richiamo
018400020517  1a C                   If        I34Ute <> *blanks or Wute <> *blanks
018500020409     C                   eval      DUtMsg = Msg(1)
018600020517     C                   Else
018700020409     C                   eval      DUtMsg = Msg(2)
018800020517     C                   Endif
018900020409      *
019000020409  1- C                   ENDSL
019100020403      *
019200020319     C                   EndSr
019300020409      *----------------------------------------------------------*
019400020409      * Dati Utente
019500020409      *----------------------------------------------------------*
019600180228     C     GesUtent      BegSr
019700020409      *
019800180228     C***                IF        Wute <> 'QPGMR      '
019900180228     c*
020000180228     C***                IF        Wute <> *blanks
020100180228     C***  Wute          chain     Utent00f                           21
020200180228     C***                ELSE
020300180228     C***  I34Cou        chain     Utent02l                           21
020400180228     C***                ENDIF
020500050405     c
020600020409      *
020700020409      *  Se ho trovato il profilo ............
020800180228  1a C***                IF        *in21 = *off
020900180228     C***                eval      WUte = UtnNam
021000180228     C***                eval      DutUte = UtnNam
021100180228     C***                move      UtnCod        DutCou
021200020509      *
021300180228  1- C***                ENDIF
021400180228  1- C***                ENDIF
021500020507      *
021600020507      * Recupero il P.O. di gestione
021700020507      *
021800020507      *  Se il profilo inizia per REMxxx il P.O corrisponde a xxx
021900180227   1aC*                  IF        %subst(Wute:1:3) = 'REM'  and
022000180227     C*                            %subst(Wute:4:3) > '000'  and
022100180227     C*                            %subst(Wute:4:1) >= '0'   and
022200180227     C*                            %subst(Wute:5:1) >= '0'   and
022300180227     C*                            %subst(Wute:6:1) >= '0'
022400180227     C*                  eval      W003A = %subst(Wute:4:3)
022500180227     C*                  movel     W003A         $PoU
022600180227   1eC*                  ELSE
022700020507      *
022800020507      *  Il P.O in questo caso è Simfel: aggancio parut
022900180228     c                   exsr      Rep§SIMFEL
023000020517      *
023100020521      * Se Simfel è a zero significa che siamo in sede.
023200020521      *   In questo caso imposto 046, ma questo caso non si verifica dato
023300020521      *   che in sede deve esistere AZUTE00F
023400180228   2aC                   IF        §§Simfel <> *zeros
023500180228     C                   move      §§Simfel      $PoU
023600180228   2eC                   ELSE
023700180228     C                   z-add     046           $PoU
023800180228   2-C                   ENDIF
023900020517      *
024000180227   1-C*                  ENDIF
024100020409      *
024200180228     C                   EndSr
024300971231      *----------------------------------------------------------*
024400011214      * Aggancio AzOrg
024500971231      *----------------------------------------------------------*
024600011217     C     GesOrg        BEGSR
024700020104      *
024800020403     C     $Pou          chain     AzOrg01l
024900020402      *
025000021129      *--Aggiorno dati per Organigramma trovato
025100021129     C                   IF        %FOUND(AZORG01L)
025200180223     C**                 z-add     wcpc          DUtKci
025300020423     C                   move      OrgFil        DUtPou
025400020405     C                   eval      DUtDPo = OrgDes
025500020405     C                   eval      DUtDis = OrgFl3
025600020410     C                   move      OrgCar        DutAre
025700020417     C                   move      OrgFel        DutFel
025800020417     C                   eval      Og143  = OrgDe3
025900020410     C                   eval      DUtDe3 = Og143
026000020410     C                   eval      DUtNtw = §ogntw
026100020417     C                   move      §OgAs         DutAsI
026200020507      *
026300021129      *--Aggiorno dati per Organigramma NON trovato
026400021129    1C                   ELSE
026500020507     C                   move      'E'           DutErr
026600020507     C                   movel     Msg(4)        DutMsg
026700020507      *
026800021129    1C                   ENDIF
026900020507      *
027000020507     C                   ENDSR
027100011217      *----------------------------------------------------------*
027200011217      * Terminal Partenza/Arrivo
027300011217      *----------------------------------------------------------*
027400011217     C     GesTPA        BegSr
027500011220      *
027600020521      *  Recupero Simfel da PARUT
027700180228     c                   exsr      Rep§SIMFEL
027800020521      *
027900020527      *  Se §§SIMFEL = 0 mi trovo sul AS di sede nel S.I. di sede
028000020527      *  imposto quindi: SIMFEL = 0;
028100020527    1C                   IF        §§SIMFEL = *zeros
028200020521     C                   eval      SimFel = §§SIMFEL
028300020527      *     Se P.O. utente non sede ricavo i terminal e il tipo p.o.
028400020527      *     Se P.O. utente è sede imposto il tipo p.o.
028500020527    2c                   IF        $POU <> 046
028600020527     C                   exsr      RepTpTa
028700020527     C                   move      d55Tfa        DutTfa
028800020527     C                   move      d55Tfp        DutTfp
028900020527     C                   move      Wdata         DutDte
029000020527   x2C                   ELSE
029100020527     C                   eval      DutLPo = 'S'
029200020527    2C                   ENDIF
029300020521      *
029400020521      *    Se §§SIMFEL <> 0 mi trovo sul AS di sede nel S.I. di P.O.
029500020527      *    o sul AS di un p.o. quindi imposto com SIMFEL il terminal
029600020527      *    partenza del p.o. UTENTE
029700020527   x1C                   ELSE
029800011220      *
029900020527      * Ricavo entrambi i terminal, Partenza ed arrivo
030000020527     C                   EXSR      RepTpTa
030100011217      *
030200020405     C                   move      d55Tfa        DutTfa
030300020405     C                   move      d55Tfp        DutTfp
030400020405     C                   move      d55Tfp        Simfel
030500020405     C                   move      Wdata         DutDte
030600020507      *
030700020527    1C                   ENDIF
030800020403      *
030900011217     C                   EndSr
031000020403      *----------------------------------------------------------*
031100020403      * Pulizia campi output
031200020403      *----------------------------------------------------------*
031300020409     C     PulDati       BEGSR
031400020409      *
031500021129      * Pulisco le dataara e i campi di output della Ds
031600020403     C                   clear                   DDatiUte
031700020409     C     *lock         in        §DatiUte
031800020409     C                   movel     DDatiUte      §DatiUte
031900020409     C                   out       §DatiUte
032000020405     C                   clear                   AzUteDs
032100020409     C     *lock         in        §AZUTE
032200020409     C                   movel     AzUteDS       §AZUTE
032300020409     C                   out       §AZUTE
032400020403      *
032500020409     C                   clear                   AzUteds
032600020409     C                   movel     AzUteDs       O34Rut
032700020405     C                   clear                   O34UTE
032800020405     C                   clear                   O34POU
032900020405     C                   clear                   O34COU
033000020405     C                   clear                   O34DPO
033100020405     C                   clear                   O34DIS
033200020405     C                   clear                   O34CAR
033300020405     C                   clear                   O34FEL
033400020417     C                   clear                   O34ASI
033500020405     C                   clear                   O34NTW
033600020405     C                   clear                   O34DE3
033700020922     C                   clear                   O34TFP
033800020922     C                   clear                   O34FLO
033900020405     C                   clear                   O34ERR
034000020405     C                   clear                   O34MSG
034100020403      *
034200020403     C                   EndSr
034300020417      *----------------------------------------------------------*
034400020417      * Reperisco Ragione sociale Utente
034500020417      *----------------------------------------------------------*
034600180227     C*    RepRsUt       BEGSR
034700020417      *
034800180227     C*                  z-add     1             ParKut
034900180227     C*                  z-add     10            ParTip
035000180227     C*    Krut          chain     Parut00f
035100180227   5aC*                  IF        %FOUND(PARUT00F)
035200180227     C*                  movel     RagSoc        RsUtOld
035300180227     C*                  movel     RagSoc        RsUt
035400180227     C*                  ENDIF
035500020417      *
035600020611      * Dopo aver determinato il p.o. utente Rsut viene truccato
035700020611      * aggiungendo il p.o..
035800020611      *
035900180227     C*                  EndSr
036000020517      *----------------------------------------------------------*
036100020517      * Reperisco SIMFEL dal file PARUT
036200020517      *----------------------------------------------------------*
036300180228     C     Rep§SIMFEL    BEGSR
036400020517      *
036500180228     C                   clear                   Ca150
036600180228     C                   z-add     1             ParKut
036700180228     C                   z-add     80            ParTip
036800180228     C     Krut          chain     Parut00f
036900180228   2aC                   IF        %FOUND(PARUT00F)
037000180228     C                   movel     Ca150         CnCr80
037100180228     C                   ELSE
037200180228     C                   clear                   Cncr80
037300180228     c                   ENDIF
037400020517      *
037500180228     C                   EndSr
037600020527      *----------------------------------------------------------*
037700020922      * Reperisco Terminal Par/Arr e livello p.o.
037800020527      *----------------------------------------------------------*
037900020527     C     RepTpTa       BEGSR
038000020527      *
038100020527     C                   clear                   Fnlv55Ds
038200020527     c                   move      'L'           d55tla
038300020527     C                   move      $Pou          d55lin
038400020527     C                   move      Wdata         d55drf
038500020527     C                   call      'FNLV55R'
038600020527     C                   parm                    FNLV55DS
038700020527      *
038800020527      * Ricavo il livello del P.O. solo se ho trovato i terminal
038900020922     C                   SELECT
039000020922     C                   WHEN      D55Err <> *blanks
039100020922     C                   WHEN      D55TFP = $POU
039200020527     C                   eval      DutLPo = '1'
039300020922     C                   OTHER
039400020527     C                   eval      DutLPo = '2'
039500020922     C                   ENDSL
039600020527      *
039700020527     C                   EndSr
039800020403      *----------------------------------------------------------*
039900020405      * Operazione di fine Pgm
040000020403      *----------------------------------------------------------*
040100020404     C     EndPgm        BEGSR
040200020507      *
040300021014      *
040400020404      * Disalloco le aree dati
040500020507     C                   UnLock(e) §AzUte
040600020507     C                   UnLock(e) §DatiUte
040700020404      *
040800020405     C                   eval      *inLR = *on
040900020403      *
041000020403     C                   EndSr
041100020403      *--------------------------------------------------------------------
041200020403      *  *inzsr Routine iniziale
041300020403      *--------------------------------------------------------------------
041400020402     C     *inzsr        BEGSR
041500020405      *
041600020920      * Richiamo trul25c per verificare se esiste AZUTE,se esiste lo apro.
041700020920      *  Se non esiste apro UTENT
041800020402     c                   call      'TRUL25C'
041900020402     c                   parm      '2'           §TipoC            1
042000020402     c                   parm      'AZUTE00F  '  §File            10
042100020402     c                   parm      '*LIBL     '  §Libr            10
042200020507     c                   parm      '*FIRST    '  §Mbr             10
042300020405     c                   parm      *blanks       §Text            10
042400020402     c                   parm      *blanks       §FrcRatio         1
042500020402     c                   parm      *blanks       §OvrScp           1
042600020402     c                   parm      *zeros        §Esito            2
042700020402      *
042800020403     C     §Esito        ifeq      '00'
042900020402     C                   open      AzUte01L
043000020403     C                   open      AzUte02L
043100180228     C*                  else
043200180228     C*                  open      UTENT00F
043300180228     C*                  open      UTENT02L
043400020920     C                   endif
043500020424      *
043600020424      * Reperisco data del giorno per calcolo terminal
043700020424     C                   TIME                    dataiso
043800020424     C     *iso          movel     dataiso       wdata
043900020402      *
044000020402      * Chiavi di lettura
044100020402     C     K02Ute        KLIST
044200020402     C                   KFLD                    I34Pou
044300020402     C                   KFLD                    I34Cou
044400020507      *
044500020403     C                   Endsr
044600020405      *----------------------------------------------------------*
044700020402**
044800020709TIBS34R-Profilo utente non codificato
044900020709TIBS34R-Codice utente non codificato
045000020709TIBS34R-Profilo utente scaduto
045100020709TIBS34R-P.O. utente non presente in organigramma
