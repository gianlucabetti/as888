000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120     D*-----------------
000500040212     D* PARAMETRI DI INPUT/OUTPUT
000600001120     D*-----------------
000700040726     D*  InTxt             => Stringa di input da trattare
000800040726     D*  InTipo            => Flag che indica il tipo della stringa da testare
000900040726     D*                       Valori: 'P' = trattasi PATH
001000040726     D*                               'F' = trattasi d Nome File
001100040726     D*  InSubPTH          => Flag che indica, in caso d "InTipo" = 'P', se il PATH può contenere subfolders
001200040726     D*                       Valori: *blanks = nn effettua alcun controllo
001300040726     D*                               'S'     = può contenere subfolders
001400040726     D*                               'N'     = NON può contenere subfolders
001500040726     D*  OutSubPTH         => Flag che indica se nella stringa di input da trattare sono presenti subfolders
001600040726     D*  OutErr            => Flag che indica la presenza d errori o esito negativo del controllo
001700040726     D*                       Valori: *blanks = tutto ok
001800040726     D*                               1       = test negativo
001900040726     D*  OutMsg            => Descrizione dell'errore
002000040212     D*
002100040726     D InTxt           S           2048
002200040726     D InTipo          S              1
002300040726     D InSubPTH        S              1
002400040726     D OutSubPTH       S              1
002500040726     D OutErr          S              1
002600040726     D OutMsg          S             80
002700040212     D*-----------------
002800040212     D* VARIABILI E CONSTANTI WRK
002900040212     D*-----------------
003000040726     D SkNomiNoOK      C                   CONST('/\:;,!"#£$%&*|=''+§°çòèéì[]{}-
003100040726     D                                     àù?"<>')
003200040726     D SkPathNoOK      C                   CONST('/?:;,!"#£$%&*|=''+§°çòèéì[]{}-
003300040726     D                                     àù"<>')
003400040726 xxx D SkSubPthNoOK    C                   CONST('\\')
003500040726     D SkSubPth        C                   CONST('\')
003600040726     D WrkSkChk        S            256    INZ(*blanks)
003700001120
003800000411
003900000414     C*------------------------------------------------------------------------*
004000000414     C* MAIN LINE
004100000414     C*------------------------------------------------------------------------*
004200040212     C                   setoff                                       55
004300040726     C*
004400040726     C* Innanzitutto inizializzo i parametri d output
004500040726     C                   eval      OutSubPTH = *blanks
004600040726     C                   eval      OutErr    = *blanks
004700040726     C                   eval      OutMsg    = *blanks
004800000414     C*
004900040726     C* Prima d tutto verifico la validità dei parametri in input
005000040726     C                   if        InTipo = 'P' OR
005100040726     C                             InTipo = 'F'
005200040212     C                   else
005300040212     C                   seton                                        55
005400040212     C                   endif
005500040726     C                   if        InSubPTH = *blanks OR
005600040726     C                             InSubPTH = 'S'     OR
005700040726     C                             InSubPTH = 'N'
005800040212     C                   else
005900040212     C                   seton                                        55
006000040212     C                   endif
006100040212     C*
006200040726     C* Se parametri input tutto ok => procedo
006300040212     C                   if        *in55 = *off
006400040212     C                   exsr      procedi
006500040212     C                   else
006600040726     C                   eval      OutErr = '1'
006700040726     C                   eval      OutMsg = 'Errori nei parametri di input'
006800040212     C                   endif
006900001120     C*
007000001120     C                   SETON                                        LR
007100001120
007200001120
007300000414
007400001120     C*------------------------------------------------------------------------*
007500040726     C* PROCEDI - OPERAZIONI DI VERIFICA CORETTEZZA FORMALE
007600001120     C*------------------------------------------------------------------------*
007700040212     C     PROCEDI       BEGSR
007800001120     C*
007900040726     C* Elaboro solo se la stringa d input NN è vuole
008000040726     C                   if        InTxt = *blanks
008100040726     C                   eval      OutErr = '1'
008200040726     C                   eval      OutMsg = 'La stringa da verificare è vuota'
008300040726     C                   else
008400040726     C*
008500040726     C* In relazione che il tipo stringa indicato nei parametri d input considero due diverse schiere
008600040726     C* x verificare i caratteri NN consentiti
008700040726     C                   if        InTipo = 'P'
008800040726     C                   eval      WrkSkChk = SkPathNoOK
008900040726     C                   endif
009000040726     C                   if        InTipo = 'F'
009100040726     C                   eval      WrkSkChk = SkNomiNoOK
009200040726     C                   endif
009300040726     C*
009400040726     C                   z-add     1             SkJ               2 0
009500040726     C                   movel     *blanks       SkChar            1
009600040726     C                   dow       SkJ <= %len(WrkSkChk)
009700040726     C                   eval      SkChar = %subst(WrkSkChk:SkJ:1)
009800040726     C                   if        %scan(SkChar:InTxt) > 0
009900040726     C                   eval      OutErr = '1'
010000040726     C                   eval      OutMsg = 'Valori non validi: ' + SkChar
010100040726     C                   leave
010200040726     C                   endif
010300040726     C                   add       1             SkJ
010400040726     C                   enddo
010500040726     C*
010600040726     C* Se tutt ok => verifico se nella stringa d input da trattare sono presenti \ indicanti subfolders
010700040726     C                   if        OutErr = *blanks
010800040726     C                   movel     *blanks       wInTxt         2048
010900040726     C                   if        %subst(%trim(InTxt):1:1) = SkSubPth
011000040726     C                   eval      wInTxt = %subst(%trim(InTxt):2)
011100040726     C                   endif
011200040726     C                   if        %subst(%trim(wInTxt):%len(%trim(wInTxt)):1)
011300040726     C                             = SkSubPth
011400040726     C                   eval      wInTxt = %subst(%trim(wInTxt):1:
011500040726     C                                      %len(%trim(wInTxt))-1)
011600040726     C                   endif
011700040726     C*
011800040726     C                   if        %scan(SkSubPth:wInTxt) > 0
011900040726     C                   eval      OutSubPTH = '1'
012000040726     C                   if        InSubPTH = 'N'
012100040726     C                   eval      OutErr = '1'
012200040726     C                   eval      OutMsg = 'Presenti subfolders'
012300040726     C                   else
012400040726     C                   if        %scan(SkSubPthNoOk:wInTxt) > 0
012500040726     C                   eval      OutErr = '1'
012600040726     C                   eval      OutMsg = 'Indicazione subfolders errata'
012700040726     C                   endif
012800040726     C                   endif
012900040726     C                   endif
013000040726     C                   endif
013100040726     C                   endif
013200040726     C*
013300001120     C                   ENDSR
013400001120     C*------------------------------------------------------------------------*
013500000411
013600001120
013700001120
013800001120
013900000414     C*------------------------------------------------------------------------*
014000000414     C* *INZSR - OPERAZIONI INIZIALI
014100000414     C*------------------------------------------------------------------------*
014200000414     C     *INZSR        BEGSR
014300001120     C*
014400000414     C*--------------------
014500000414     C* RICEVIMENTO PARAMETRI
014600000414     C*--------------------
014700000414     C     *ENTRY        PLIST
014800040726     C                   PARM                    InTxt
014900040726     C                   PARM                    InTipo
015000040726     C                   PARM                    InSubPTH
015100040726     C                   PARM                    OutSubPTH
015200040726     C                   PARM                    OutErr
015300040726     C                   PARM                    OutMsg
015400000414     C*
015500000414     C                   ENDSR
