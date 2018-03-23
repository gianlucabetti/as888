000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120     D*-----------------
000500040212     D* PARAMETRI DI INPUT/OUTPUT
000600001120     D*-----------------
000700040212     D*  TxtInOut          => Stringa di input/output da trattare
000800040212     D*  ElencoChar        => Elenco dei caratteri da testare
000900040226     D*                       Valori: *blanks = utilizzare default ovvero tutti i caratteri della costante <CharOkDFT>
001000040212     D*                               <elenco caratteri>
001100040212     D*  TipoElenco        => Flag che indica se l'elenco caratteri precedente deve essere considerato come
001200040212     D*                       elenco caratteri consentiti o come elenco caratteri NN consentiti
001300040212     D*                       Valori: *blanks = elenco caratteri consentiti
001400040212     D*                               1       = elenco caratteri NN consentiti
001500040212     D*  CharSost          => Carattere sostituente i caratteri NN validi della stringa d input
001600040212     D*  UpperCase         => Flag che indica se è richiesta la conversine MAIUSCOLO/minuscolo
001700040212     D*                       Valori: *blanks = NON effettuare nessuna conversione
001800040212     D*                               1       = SI  output tutto MAIUSCOLO
001900040212     D*                               2       = SI  output tutto minuscolo
002000040212     D*  ChkNull           => Flag che determina se trattare i byte NULL
002100040212     D*                       Valori: *blanks = NON effettuare controllo dei byte NULL
002200040212     D*                               1       = SI  effettuare controllo dei byte NULL
002300040212     D*  CharNull          => Carattere sostituente i byte NULL
002400040212     D*  Esito             => Valori: 0 = Nessuna modifica effettuata alla stringa d input
002500040212     D*                               1 = Stringa d input modificata
002600040212     D*                               I = Errore nei parametri d input
002700040212     D*                               E = Errore in elaborazoine
002800040212     D*
002900040212     D TxtInOut        S           2048
003000040212     D ElencoChar      S            256
003100040212     D TipoElenco      S              1
003200040212     D CharSost        S              1
003300040212     D UpperCase       S              1
003400040212     D ChkNull         S              1
003500040212     D CharNull        S              1
003600040212     D Esito           S              1
003700040212     D*-----------------
003800040212     D* VARIABILI E CONSTANTI WRK
003900040212     D*-----------------
004000040212     D i               S              4  0
004100040212     D SavTxtInOut     S           2048
004200040212     D minu            C                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto inglese
004300040212     D maiu            C                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO INGLESE
004400040212     D CharOkDFT       C                   const('QWERTYUIOPASDFGHJKLZXCVBNM-
004500040212     D                                            qwertyuiopasdfghjklzxcvbnm-
004600040226     D                                            1234567890.,;:-_/\()!£$%&=-
004700040302     D                                            ?+*§@<>''"')
004800060213     D chAcc           C                   const('àèéìòù°ÀÁÂÃÄÅÆÈÉÊËÌÍÎÏÒÓÔÕ-   *lettere accentate e varie
004900060213     D                                            ÖÙÚÛÜàáâãäåèéêëìíîïòóôõöùú-
005000070711     D                                            ûüæçðñÇ')
005100060213     D chNoAcc         C                   const('aeeiouoAAAAAAAEEEEIIIIOOOO-   *lettere accentate e varie
005200060213     D                                            OUUUUaaaaaaeeeeiiiiooooouu-
005300070711     D                                            uuacanC')
005400001120
005500000411
005600000414     C*------------------------------------------------------------------------*
005700000414     C* MAIN LINE
005800000414     C*------------------------------------------------------------------------*
005900040212     C                   setoff                                       55
006000000414     C*
006100040212     C* Prima d tutto verifico le richieste dei parametri in input
006200040212     C                   if        TipoElenco = *blanks OR
006300040212     C                             TipoElenco = '1'
006400040212     C                   else
006500040212     C                   seton                                        55
006600040212     C                   endif
006700040212     C                   if        UpperCase  = *blanks OR
006800040212     C                             UpperCase  = '1'     OR
006900040212     C                             UpperCase  = '2'
007000040212     C                   else
007100040212     C                   seton                                        55
007200040212     C                   endif
007300040212     C                   if        ChkNull    = *blanks OR
007400040212     C                             ChkNull    = '1'
007500040212     C                   else
007600040212     C                   seton                                        55
007700040212     C                   endif
007800040212     C                   if        CharSost   = X'00'
007900040212     C                   seton                                        55
008000040212     C                   endif
008100040212     C                   if        CharNull   = X'00'
008200040212     C                   seton                                        55
008300040212     C                   endif
008400040212     C*
008500040212     C* Se input tutto ok => procedo
008600040212     C                   if        *in55 = *off
008700040212     C                   exsr      procedi
008800040212     C                   else
008900040212     C                   eval      Esito = 'I'
009000040212     C                   endif
009100001120     C*
009200001120     C                   SETON                                        LR
009300001120
009400001120
009500000414
009600001120     C*------------------------------------------------------------------------*
009700040212     C* PROCEDI - OPERAZIONI DI "BONIFICAZIONE" STRINGA
009800001120     C*------------------------------------------------------------------------*
009900040212     C     PROCEDI       BEGSR
010000040212     C*
010100040212     C* Innanzitutti salvo la stringa d input così come ricevuta prima dell'elaborazione
010200040212     C                   eval      SavTxtInOut = TxtInOut
010300001120     C*
010400040212     C* Verifico se richiesti caratteri particolari da considerare
010500040212     C                   if        ElencoChar = *blanks
010600040212     C                   eval      ElencoChar = CharOkDFT
010700040212     C                   endif
010800040226     C*
010900040226     C* Come prima cosa converto le lettere accentate nelle relative NN accentate
011000040226     C     chAcc:chNoAcc XLATE     TxtInOut      TxtInOut                       *accentate -> NO accentate
011100040212     C*
011200040212     C* Effettuo lo "bonificazione" vera e propria della stringa d input
011300040212     C                   z-add     1             i
011400040212     C                   dow       i <= %len(%trimr(TxtInOut))
011500051222     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione => gestisco sempre
011600051222     C                   if        %subst(TxtInOut:i:1) < X'40'
011700051222     C                   eval      %subst(TxtInOut:i:1) = *blanks
011800051222     C                   endif
011900040212     C* Se elaborazione x "caratteri consentiti"
012000040212     C                   if        TipoElenco = *blanks
012100040212     C                   if        %scan(%subst(TxtInOut:i:1):ElencoChar) = 0
012200040212     C                   eval      %subst(TxtInOut:i:1) = CharSost
012300040212     C                   endif
012400040212     C                   endif
012500040212     C* Se elaborazione x "caratteri NN consentiti"
012600040212     C                   if        TipoElenco = '1'
012700040212     C                   if        %scan(%subst(TxtInOut:i:1):ElencoChar) > 0
012800040212     C                   eval      %subst(TxtInOut:i:1) = CharSost
012900040212     C                   endif
013000040212     C                   endif
013100040212     C                   eval      i = i + 1
013200040212     C                   enddo
013300040212     C*
013400040212     C* Verifico se richiesto la sostituzione dei byte a null
013500040212     C                   if        ChkNull = '1'
013600040212     C                   z-add     1             i
013700040212     C                   dow       i <= %len(%trimr(TxtInOut))
013800040212     C                   if        %subst(TxtInOut:i:1) = X'00'
013900040212     C                   eval      %subst(TxtInOut:i:1) = CharNull
014000040212     C                   endif
014100040212     C                   eval      i = i + 1
014200040212     C                   enddo
014300040212     C                   endif
014400040212     C*
014500040212     C* Infine se richiesto effettuo la conversione minuscolo/MAIUSCOLO o viceversa
014600040212     C                   if        UpperCase = '1'
014700040212     C     minu:maiu     XLATE     TxtInOut      TxtInOut                       *Minuscolo -> MAIUSCOLO
014800040212     C                   endif
014900040212     C                   if        UpperCase = '2'
015000040212     C     maiu:minu     XLATE     TxtInOut      TxtInOut                       *MAIUSCOLO -> Minuscolo
015100040212     C                   endif
015200040212     C*
015300040212     C* Verifico se effettuati combiamenti sulla stringa d input iniziale
015400040212     C                   if        SavTxtInOut <> TxtInOut
015500040212     C                   eval      Esito = '1'
015600040212     C                   else
015700040212     C                   eval      Esito = '0'
015800040212     C                   endif
015900001120     C*
016000001120     C                   ENDSR
016100001120     C*------------------------------------------------------------------------*
016200000411
016300001120
016400001120
016500001120
016600000414     C*------------------------------------------------------------------------*
016700000414     C* *INZSR - OPERAZIONI INIZIALI
016800000414     C*------------------------------------------------------------------------*
016900000414     C     *INZSR        BEGSR
017000001120     C*
017100000414     C*--------------------
017200000414     C* RICEVIMENTO PARAMETRI
017300000414     C*--------------------
017400000414     C     *ENTRY        PLIST
017500040212     C                   PARM                    TxtInOut
017600040212     C                   PARM                    ElencoChar
017700040212     C                   PARM                    TipoElenco
017800040212     C                   PARM                    CharSost
017900040212     C                   PARM                    UpperCase
018000040212     C                   PARM                    ChkNull
018100040212     C                   PARM                    CharNull
018200040212     C                   PARM                    Esito
018300000414     C*
018400000414     C                   ENDSR
