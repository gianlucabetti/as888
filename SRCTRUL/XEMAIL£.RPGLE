000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120     D*-----------------
000500001120     D* VARIABILI DI WRK
000600001120     D*-----------------
000700040827     D Indice          S              5i 0
000800040827     D NumOK           C                   CONST('1234567890')
000900040827     D MaiuOK          C                   CONST('QWERTYUIOPASDFGHJKLZXCVBNM ')
001000040827     D MinuOK          C                   CONST('qwertyuiopasdfghjklzxcvbnm ')
001100161115     D SimbolOK        C                   CONST('_+-.§@&;')
001200040827     D CharOK          S            256    VARYING INZ(' ')
001300040827     D WstrIND         S            256    VARYING INZ(' ')
001400040827     D SingleChar      S              1
001500040827     D EmlPosDa        S              3  0 INZ(1)
001600040827     D EmlPos          S              3  0 INZ(*zeros)
001700040827     D EmlMail         S            100    INZ(*blanks)
001800140722     D sav§EMLINDI     S                   LIKE(§EMLINDI) INZ(*blanks)
001900140722     D sav§EMLINDO     S                   LIKE(§EMLINDO) INZ(*blanks)
002000140722
002100140722
002200140722     D*------------------
002300140722     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
002400140722     D*------------------
002500140722     D InStringa       S          65535A   VARYING                              (stringa input)
002600140722     D InSepara        S             10A                                        (separatore)
002700140722     D InTypeOut       S              1A                                        (tipo output)
002800140722     D wSkParam        S          65535A                                        (output)
002900140722     D OutErrore       S              1A                                        (flag errore)
003000140722     D OutMsg          S             80A                                        (messaggio errore)
003100161019     D SkSplitCSV_5    S            256    DIM(256)
003200140722
003300001120
003400001120     D*-----------------
003500001120     D* DS DI PROCEDURA
003600001120     D*-----------------
003700001120     D DSEMAIL       E DS
003800001120
003900000411
004000000414     C*------------------------------------------------------------------------*
004100000414     C* MAIN LINE
004200000414     C*------------------------------------------------------------------------*
004300040827     C*
004400161019     C* Inizializzo flag e messaggio d errore
004500040827     C                   MOVEL     *blanks       §EMLERRO
004600040827     C                   MOVEL     *blanks       §EMLMSGO
004700040827     C*
004800040827     C* Inizializzo flag d erorre generale (serve in caso d controllo <n> indirizzi e-mail)
004900040827     C                   MOVEL     'S'           FlgOK             1
005000041215     C*
005100041215     C* Quindi verifico subito se il parametro contenente la stringa da verificare è valorizzato
005200041215     C                   IF        §EMLINDI = *blanks
005300041215     C                   MOVEL     'N'           FlgOK
005400041215     C                   EVAL      §EMLMSGO = 'E-mail errato: ' +
005500041215     C                                      'indirizzo non valorizzato'
005600041215     C                   ELSE
005700161019     C*
005800161019     C* Sostituisco 2 simboli che sono (erroneamente) utilizzati per separare indirizzi e-mail
005900161019     C* con il carattere corretto
006000161019     C                   EVAL      §EMLINDI = %scanrpl(',':';':§EMLINDI)
006100161019     C                   EVAL      §EMLINDI = %scanrpl('|':';':§EMLINDI)
006200040827     C*
006300040827     C* Effettuo controllo su l'elenco completo dei caratteri validi in un indirizzo e-mail
006400040827     C                   EVAL      CharOK = NumOK + MinuOK + MaiuOK + SimbolOK
006500040827     C                   EVAL      WstrIND = §EMLINDI
006600040827     C                   Z-ADD     1             Indice
006700040827     C                   DOW       Indice<=%len(%trim(WstrIND))
006800040827     C                   EVAL      SingleChar=%subst(WstrIND:Indice:1)
006900040827     C     SingleChar    SCAN      CharOK
007000040827     C                   IF        (NOT %found)
007100040827     C                   MOVEL     '1'           §EMLERRO
007200040827     C                   EVAL      §EMLMSGO = 'E-mail errato: ' +
007300040827     C                                      'presenti caratteri non validi '+
007400040827     C                                      '('+ SingleChar + ')'
007500040827     C                   LEAVE
007600040827     C                   ENDIF
007700040827     C                   ADD       1             Indice
007800040827     C                   ENDDO
007900040827     C*
008000040827     C* Effettuo controlli su correttezza formale singoli indirizzi e-mail
008100040827     C                   IF        §EMLERRO = *blanks
008200040827     C                   EVAL      EmlMail = %trim(§EMLINDI)
008300040827     C                   EVAL      EmlPos = *zeros
008400040827     C                   EVAL      EmlPosDa = 1
008500170120     C*
008600040827     C* Innanzitutto verifico che nn inizi con il carattere separatore indirizzi e-mail
008700040827     C                   DOW       %subst(EmlMail:1:1) = ';'
008800040827     C                   EVAL      %subst(EmlMail:1:1) = *blanks
008900040827     C                   EVAL      EmlMail = %trim(EmlMail)
009000040827     C                   ENDDO
009100170120     C*
009200170120     C* Se a questo punto email ancora valorizzata => proseguo
009300170120     C                   IF        %len(%trim(EmlMail)) = *zeros
009400170120     C                   MOVEL     'N'           FlgOK
009500170120     C                   EVAL      §EMLMSGO = 'E-mail errato: ' +
009600170120     C                                      'indirizzo non valorizzato'
009700170120     C                   ELSE
009800170120     C*
009900040827     C* Poi verifico che nn finisca con il carattere separatore indirizzi e-mail
010000040827     C                   DOW       %subst(EmlMail:%len(%trim(EmlMail)):1) = ';'
010100040827     C                   EVAL      %subst(EmlMail:%len(%trim(EmlMail)):1)=*blanks
010200040827     C                   EVAL      EmlMail = %trim(EmlMail)
010300040827     C                   ENDDO
010400140722     C*
010500140722     C* Quindi chiamo il driver di controllo formale indirizzo email per ogni indirizzo email in input
010600140722     C                   eval      InStringa = %trim(EmlMail)
010700140722     C*
010800140722     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
010900140722     C                   CALL      'XSPLIT2'
011000140722     C                   PARM                    InStringa
011100140722     C                   PARM      ';'           InSepara
011200161019     C                   PARM      '5'           InTypeOut
011300140722     C                   PARM      ''            wSkParam
011400140722     C                   PARM                    OutErrore
011500140722     C                   PARM                    OutMsg
011600161019     C                   MOVEA     wSkParam      SkSplitCSV_5
011700140722     C*
011800140722     C* Valorizzo la schiera delle intestazioni colonne (elementi)
011900140722     C                   z-add     1             x                 4 0
012000161019     C                   dow       x <= %elem(SkSplitCSV_5)
012100161019     C                   if        SkSplitCSV_5(x) = *blanks
012200161019     C***                leave
012300140722     C                   else
012400140722     C                   clear                   DSEMAIL
012500161019     C                   eval      §EMLINDI = %trim(SkSplitCSV_5(x))
012600140722     C                   call      'XEMAIL1'
012700140722     C                   parm                    DSEMAIL
012800140722     C                   if        §EMLERRO = '1'
012900140722     C                   movel     'N'           FlgOK
013000161019     C***                leave
013100140722     C                   else
013200140722     C                   if        x > 1
013300140722     C                   eval      sav§EMLINDO = %trim(sav§EMLINDO) + ';' +
013400140722     C                                           %trim(§EMLINDO)
013500140722     C                   else
013600140722     C                   eval      sav§EMLINDO = %trim(§EMLINDO)
013700140722     C                   endif
013800140722     C                   endif
013900140722     C                   endif
014000140722     C                   add       1             x
014100140722     C                   enddo
014200170120     C*
014300170120     C                   ENDIF
014400041215     C*
014500041215     C                   ENDIF
014600041215     C*
014700041215     C* Al termine se rilevati errori emetto messaggio relativo
014800040827     C                   IF        FlgOk = 'N' OR
014900040827     C                             %len(%trim(EmlMail)) = 1
015000040827     C                   MOVEL     '1'           §EMLERRO
015100140722     C                   IF        §EMLMSGO=*blanks
015200140722     C                   EVAL      §EMLMSGO='Indirizzo email formalmente errato'
015300060316     C                   ENDIF
015400040827     C                   ELSE
015500040827     C                   ENDIF
015600161019     C                   EVAL      §EMLINDO = sav§EMLINDO
015700040827     C                   ENDIF
015800040827     C*
015900140722     C                   EVAL      §EMLINDI = sav§EMLINDI
016000140722     C*
016100040827     C                   SETON                                        LR
016200001120     C*------------------------------------------------------------------------*
016300000411
016400001120
016500001120
016600001120
016700000414     C*------------------------------------------------------------------------*
016800000414     C* *INZSR - OPERAZIONI INIZIALI
016900000414     C*------------------------------------------------------------------------*
017000000414     C     *INZSR        BEGSR
017100001120     C*
017200000414     C*--------------------
017300000414     C* RICEVIMENTO PARAMETRI
017400000414     C*--------------------
017500000414     C     *ENTRY        PLIST
017600001120     C                   PARM                    DSEMAIL
017700140722     C*
017800140722     C* Salvo i parametri originali
017900140722     C                   EVAL      sav§EMLINDI = §EMLINDI
018000120514     C*
018100161019     C* Epuro subito dagli spazi inutili
018200161019     C                   EVAL      §EMLINDI = %scanrpl(' ':'':§EMLINDI)
018300001120     C*
018400000414     C                   ENDSR
