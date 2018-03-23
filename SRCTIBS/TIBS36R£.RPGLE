000100020103     H DECEDIT('0,') DATEDIT(*DMY.)
000200020419
000300020419      *--------------------------------------------------------------*
000400011214      *      UTILITY DI CONTROLLO PER PROFILI UTENTE                 *
000500020419      *--------------------------------------------------------------*
000600020419      *      FORZATO IL CAPOCONTO CLIENTI 151  (Wcpc)                *
000700020419      *--------------------------------------------------------------*
000800020419
000900020314     FAzUte01L  IF   E           K DISK    usropn
001000020920     FAzUte02L  IF   E           K DISK    usropn  rename(AzUte000:AzUte02)
001100020920     FUtent00f  IF   E           K DISK    usropn
001200020920     FUtent02l  IF   E           K DISK    usropn  rename(Utent000:Utent02)
001300020409     FAzOrg01L  IF   E           K DISK
001400020409      *
001500020419      * Definisco Parut00f come file esterno. Quindi per la chain
001600020419      * devo definire la chiave come DS
001700020409     FPARUT00F  IF   F  163     3AIDISK    KEYLOC(11)
001800020419      *--------------
001900020409     D KRut            DS
002000020409     D  ParKut                 1      1P 0
002100020409     D  ParTip                 2      3P 0
002200971210      *--------------------------------------------------------------*
002300011217      * Ds per reperimento terminal di linea
002400011214     D FnLv55Ds      E DS
002500011217      *--------
002600011217      * Ds di riferimento al file esterno AzUte00F
002700020405     D AzuteDs       E DS                  ExtName(AzUte00F)
002800020319      *--------
002900020319      * Ds per dati organigramma
003000020405     D DDatiUte      E DS
003100020219      *--------
003200020219      * Descrizione dell'organigramma
003300020314     D Og143         E DS
003400020405     D CNCR80        E DS                  prefix(§§)
003500021015     D TIBS36DS      E DS
003600020314      *---------------------
003700020419      * ricavo profilo del JOB
003800020408     DINFDS           SDS
003900020419     D USER                  254    263
004000020408      *---------------------
004100020409     D w003A           s              3
004200020409     D $Pou            s              3  0
004300020409     D $noAzUte        s              1
004400020409     D dataiso         s               d   datfmt(*iso)
004500020409     D WDATA           s              8  0
004600021015     d Wute            s                   like(I36ute)
004700020419     d Wcpc            s                   like(DutKci)  INZ(0151)
004800020402      *--------------------------------------------------------------*
004900020424     D Msg             S             78    DIM(04) CTDATA PERRCD(1)
005000020409      *--------------------------------------------------------------*
005100020417     IPARUT00F  AA  01
005200020409     I                                 14  163  CA150
005300020417     I                                 14   33  RagSoc
005400971212      *--------------------------------------------------------------*
005500011217      *  MAIN
005600971210      *--------------------------------------------------------------*
005700020402      *
005800020402      * Ricevo parametri in entrata
005900020402     C     *ENTRY        PLIST
006000021015     C                   PARM                    Tibs36Ds
006100020405      * Pulizia dei campi di output
006200020409     C                   Exsr      PulDati
006300020405      *
006400020405      * Se non richiesta fine elaborazione
006500021015    1C                   IF        I36Tla <> 'C'
006600020402      * Tipo lancio
006700021015      * Se non impostato correttamente Tipo lancio chiudo in LR
006800020402      *
006900021015    2C                   IF        I36Tla <> ' ' and  I36Tla <> 'L'
007000021015     C                   eval      I36Tla ='L'
007100020507   2-C                   ENDIF
007200020409      *
007300021015      * Se fra i dati di Input non ho ricevuto nè il Profilo, nè il P.O.,
007400020409      * nè il Cod.Utente, imposto il Profilo uguale a quello del job
007500021015    2C                   IF        I36Ute = *blanks
007600021015     C                             and I36Pou = *zeros and I36Cou = *zeros
007700020419     C                   eval      Wute = User
007800020419     c                   ELSE
007900021015     C                   eval      Wute = I36ute
008000020409   2-C                   ENDIF
008100020405      *
008200020417      * RECUPERO DATI UTENTE
008300020417      * Distinguo i due casi:Se ho AzUte lo leggo,altrimenti uso Utent
008400020409   2aC                   IF        %OPEN(AZUTE01L)
008500020405     C                   Exsr      GesAzUte
008600020507   2eC                   ELSE
008700020409     C                   Exsr      GesUtent
008800020417   2-C                   ENDIF
008900020424      *
009000020507      * RECUPERO DATI DA AZORG
009100020507      *  Se non esiste AzUte oppure se esite AzUte e non sono in errore
009200021015   2aC                   IF        NOT %OPEN(AZUTE01L) or   O36Err<>'E'
009300020402     C                   Exsr      GesOrg
009400020424   2-C                   ENDIF
009500020403      *
009600021015      *  Ricavo il terminal partenza
009700021015    2C                   IF        O36err = *blanks  and
009800021015     C                             O36pou <> *zeros  and  O36pou <> 046
009900020922     C                   exsr      RepTp
010000020922    2C                   ENDIF
010100020404      *
010200020405    1C                   ENDIF
010300020403      *
010400020403      * Operazioni finali
010500021015     C     I36Tla        ifne      ' '
010600021015     C                   eval      *inLR = *on
010700021015     C                   else
010800021015     C                   eval      *inRT = *on
010900021015     C                   endif
011000020319      *----------------------------------------------------------*
011100020319      * Dati Utente
011200020319      *----------------------------------------------------------*
011300020405     C     GesAzUte      BegSr
011400020319      *
011500020419  1a C                   IF        WUte  <> *blanks
011600020419     C     Wute          chain     AzUte01L                           21
011700020409     C                   ELSE
011800020409     C     K02Ute        chain     AzUte02l                           21
011900020409  1- C                   ENDIF
012000020409      *
012100020419      * Se ho trovato il profilo ............
012200020405  1a C                   IF        *in21 = *off
012300020409     C                   clear                   $NoAzUte
012400020419      *     imposto il P.O di gestione per agganciare AZORG
012500020403     C                   move      UteFil        $Pou
012600020405      * Segnalo che non ho trovato l'utente
012700020419  1e C                   ELSE
012800020409     C                   move      '1'           $NoAzUte
012900020405  1- C                   ENDIF
013000020409      *
013100021015      * Aggiorno dati in DS in base a:
013200020517      *    - trovato profilo scaduto nel file
013300020517      *    - trovato profilo valido nel file
013400020517      *    - non trovato profilo valido nel file
013500020409  1  C                   SELECT
013600020409      *
013700021015      *-- PROFILO: trovato, scaduto
013800021015     C                   WHEN      $NoAzUte = ' '  and  Wdata > UteDsc
013900021015     C                   eval      O36Err = 'E'
014000021015     C                   eval      O36Msg = Msg(3)
014100020517      *
014200021015      *-- PROFILO: trovato, non scaduto
014300021015     C                   WHEN      $NoAzUte = ' '
014400021015     C                   eval      O36Ute = UteUte
014500021015     C                   move      UteFil        O36Pou
014600021015     C                   move      UteCdu        O36Cou
014700021015     C                   movel     AzUteDs       O36Rut
014800020409      *
014900021015      *-- PROFILO: non trovato
015000020405     C                   OTHER
015100021015     C                   eval      O36Err = 'E'
015200020517      *       Imposto msg in base al tipo richiamo
015300021015  1a C                   if        I36Ute <> *blanks or Wute <> *blanks
015400021015     C                   eval      O36Msg = Msg(1)
015500020409     C                   else
015600021015     C                   eval      O36Msg = Msg(2)
015700020409  2- C                   endif
015800020409      *
015900020409  1- C                   ENDSL
016000020403      *
016100020319     C                   EndSr
016200020409      *----------------------------------------------------------*
016300020409      * Dati Utente
016400020409      *----------------------------------------------------------*
016500020409     C     GesUtent      BegSr
016600020409      *
016700020419     C                   IF        Wute <> *blanks
016800020419     C     Wute          chain     Utent00f                           21
016900020409     C                   ELSE
017000021015     C     I36Cou        chain     Utent02l                           21
017100020409     C                   ENDIF
017200020409      *
017300021015      *  Se ho trovato il profilo aggiorno dati DS
017400020409  1a C                   IF        *in21 = *off
017500020419     C                   eval      WUte = UtnNam
017600021015     C                   eval      O36Ute = UtnNam
017700021015     C                   move      UtnCod        O36Cou
017800020409  1- C                   ENDIF
017900020507      *
018000020507      * Recupero il P.O. di gestione
018100020507      *
018200020507      *  Se il profilo inizia per REMxxx il P.O corrisponde a xxx
018300020507   1aC                   IF        %subst(Wute:1:3) = 'REM'  and
018400020507     C                             %subst(Wute:4:3) > '000'  and
018500020507     C                             %subst(Wute:4:1) >= '0'   and
018600020507     C                             %subst(Wute:5:1) >= '0'   and
018700020507     C                             %subst(Wute:6:1) >= '0'
018800020507     C                   eval      W003A = %subst(Wute:4:3)
018900020507     C                   movel     W003A         $PoU
019000020507   1eC                   ELSE
019100020507      *
019200020507      *  Il P.O in questo caso è Simfel: aggancio parut
019300020517     c                   exsr      Rep§SIMFEL
019400020517      *
019500020521      * Se Simfel è a zero significa che siamo in sede.
019600020521      *   In questo caso imposto 046, ma questo caso non si verifica dato
019700020521      *   che in sede deve esistere AZUTE00F
019800020517   2aC                   IF        §§Simfel <> *zeros
019900020507     C                   move      §§Simfel      $PoU
020000020517   2eC                   ELSE
020100020507     C                   z-add     046           $PoU
020200020517   2-C                   ENDIF
020300020517      *
020400020507   1-C                   ENDIF
020500020409      *
020600020409     C                   EndSr
020700971231      *----------------------------------------------------------*
020800011214      * Aggancio AzOrg
020900971231      *----------------------------------------------------------*
021000011217     C     GesOrg        BEGSR
021100020104      *
021200020403     C     $Pou          chain     AzOrg01l
021300020507      *
021400021015      *-- Aggiorno dati Organigramma trovato
021500021015    1C                   IF         %FOUND(AZORG01L)
021600021015     C                   move      $Pou          O36Pou
021700021015     C                   eval      O36DPo = OrgDes
021800021015     C                   eval      O36Dis = OrgFl3
021900021015     C                   eval      O36Car = OrgCar
022000021015     C                   move      OrgFel        O36Fel
022100021015     C                   eval      O36De3 = OrgDe3
022200020417     C                   eval      Og143  = OrgDe3
022300021015     C                   eval      O36Ntw = §OgNtw
022400021015     C                   move      §OgAs         O36AsI
022500020507      *
022600020507      *--Aggiorno dati per DS e Organigramma NON trovato
022700021015    1C                   ELSE
022800021015     C                   move      $Pou          O36Pou
022900021015     C                   move      'E'           O36Err
023000021015     C                   movel     Msg(4)        O36Msg
023100020507      *
023200021015    1C                   ENDIF
023300020507      *
023400020507     C                   ENDSR
023500020403      *----------------------------------------------------------*
023600020403      * Pulizia campi output
023700020403      *----------------------------------------------------------*
023800020409     C     PulDati       BEGSR
023900020409      *
024000020409     C                   clear                   AzUteds
024100021015     C                   movel     AzUteDs       O36Rut
024200021015     C                   clear                   O36UTE
024300021015     C                   clear                   O36POU
024400021015     C                   clear                   O36COU
024500021015     C                   clear                   O36DPO
024600021015     C                   clear                   O36DIS
024700021015     C                   clear                   O36CAR
024800021015     C                   clear                   O36FEL
024900021015     C                   clear                   O36ASI
025000021015     C                   clear                   O36NTW
025100021015     C                   clear                   O36DE3
025200021015     C                   clear                   O36TFP
025300021015     C                   clear                   O36FLO
025400021015     C                   clear                   O36ERR
025500021015     C                   clear                   O36MSG
025600020403      *
025700020403     C                   EndSr
025800020517      *----------------------------------------------------------*
025900020517      * Reperisco SIMFEL dal file PARUT
026000020517      *----------------------------------------------------------*
026100020517     C     Rep§SIMFEL    BEGSR
026200020517      *
026300020517     C                   clear                   Ca150
026400020517     C                   z-add     1             ParKut
026500020517     C                   z-add     80            ParTip
026600020517     C     Krut          chain     Parut00f
026700020517   2aC                   IF        %FOUND(PARUT00F)
026800020517     C                   movel     Ca150         CnCr80
026900020517     C                   ELSE
027000020517     C                   clear                   Cncr80
027100020517     c                   ENDIF
027200020517      *
027300020517     C                   EndSr
027400020922      *----------------------------------------------------------*
027500020922      * Reperisco Terminal Partenza
027600020922      *----------------------------------------------------------*
027700020922     C     RepTp         BEGSR
027800020922      *
027900020922     C                   clear                   Fnlv55Ds
028000020922     c                   move      'L'           d55tla
028100020922     c                   move      'P'           d55tpt
028200021015     C                   move      O36POU        d55lin
028300020922     C                   move      Wdata         d55drf
028400020922     C                   call      'FNLV55R'
028500020922     C                   parm                    FNLV55DS
028600020922      *
028700020923     C                   IF        D55Err = *blanks
028800021015     C                   move      d55Tfp        O36Tfp
028900020923     C                   ENDIF
029000020922      *
029100020922     C                   EndSr
029200020403      *--------------------------------------------------------------------
029300020403      *  *inzsr Routine iniziale
029400020403      *--------------------------------------------------------------------
029500020402     C     *inzsr        BEGSR
029600020405      *
029700020920      * Richiamo trul25c per verificare se esiste AZUTE,se esiste lo apro.
029800020920      *  Se non esiste apro UTENT
029900020402     c                   call      'TRUL25C'
030000020402     c                   parm      '2'           §TipoC            1
030100020402     c                   parm      'AZUTE00F  '  §File            10
030200020402     c                   parm      '*LIBL     '  §Libr            10
030300020507     c                   parm      '*FIRST    '  §Mbr             10
030400020405     c                   parm      *blanks       §Text            10
030500020402     c                   parm      *blanks       §FrcRatio         1
030600020402     c                   parm      *blanks       §OvrScp           1
030700020402     c                   parm      *zeros        §Esito            2
030800020402      *
030900020403     C     §Esito        ifeq      '00'
031000020402     C                   open      AzUte01L
031100020403     C                   open      AzUte02L
031200020920     C                   else
031300020920     C                   open      UTENT00F
031400020920     C                   open      UTENT02L
031500020920     C                   endif
031600020424      *
031700020424      * Reperisco data del giorno per calcolo terminal
031800020424     C                   TIME                    dataiso
031900020424     C     *iso          movel     dataiso       wdata
032000020402      *
032100020402      * Chiavi di lettura
032200020402     C     K02Ute        KLIST
032300021015     C                   KFLD                    I36Pou
032400021015     C                   KFLD                    I36Cou
032500020507      *
032600020403     C                   Endsr
032700020405      *----------------------------------------------------------*
032800020402**
032900021015TIBS36R-Profilo utente non codificato
033000021015TIBS36R-Codice utente non codificato
033100021015TIBS36R-Profilo utente scaduto
033200021015TIBS36R-P.O. utente non presente in organigramma
