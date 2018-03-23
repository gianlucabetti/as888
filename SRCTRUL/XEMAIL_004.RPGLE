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
008500040827     C* Innanzitutto verifico che nn inizi con il carattere separatore indirizzi e-mail
008600040827     C                   DOW       %subst(EmlMail:1:1) = ';'
008700040827     C                   EVAL      %subst(EmlMail:1:1) = *blanks
008800040827     C                   EVAL      EmlMail = %trim(EmlMail)
008900040827     C                   ENDDO
009000040827     C* Poi verifico che nn finisca con il carattere separatore indirizzi e-mail
009100040827     C                   DOW       %subst(EmlMail:%len(%trim(EmlMail)):1) = ';'
009200040827     C                   EVAL      %subst(EmlMail:%len(%trim(EmlMail)):1)=*blanks
009300040827     C                   EVAL      EmlMail = %trim(EmlMail)
009400040827     C                   ENDDO
009500140722     C*
009600140722     C* Quindi chiamo il driver di controllo formale indirizzo email per ogni indirizzo email in input
009700140722     C                   eval      InStringa = %trim(EmlMail)
009800140722     C*
009900140722     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
010000140722     C                   CALL      'XSPLIT2'
010100140722     C                   PARM                    InStringa
010200140722     C                   PARM      ';'           InSepara
010300161019     C                   PARM      '5'           InTypeOut
010400140722     C                   PARM      ''            wSkParam
010500140722     C                   PARM                    OutErrore
010600140722     C                   PARM                    OutMsg
010700161019     C                   MOVEA     wSkParam      SkSplitCSV_5
010800140722     C*
010900140722     C* Valorizzo la schiera delle intestazioni colonne (elementi)
011000140722     C                   z-add     1             x                 4 0
011100161019     C                   dow       x <= %elem(SkSplitCSV_5)
011200161019     C                   if        SkSplitCSV_5(x) = *blanks
011300161019     C***                leave
011400140722     C                   else
011500140722     C                   clear                   DSEMAIL
011600161019     C                   eval      §EMLINDI = %trim(SkSplitCSV_5(x))
011700140722     C                   call      'XEMAIL1'
011800140722     C                   parm                    DSEMAIL
011900140722     C                   if        §EMLERRO = '1'
012000140722     C                   movel     'N'           FlgOK
012100161019     C***                leave
012200140722     C                   else
012300140722     C                   if        x > 1
012400140722     C                   eval      sav§EMLINDO = %trim(sav§EMLINDO) + ';' +
012500140722     C                                           %trim(§EMLINDO)
012600140722     C                   else
012700140722     C                   eval      sav§EMLINDO = %trim(§EMLINDO)
012800140722     C                   endif
012900140722     C                   endif
013000140722     C                   endif
013100140722     C                   add       1             x
013200140722     C                   enddo
013300041215     C*
013400041215     C                   ENDIF
013500041215     C*
013600041215     C* Al termine se rilevati errori emetto messaggio relativo
013700040827     C                   IF        FlgOk = 'N' OR
013800040827     C                             %len(%trim(EmlMail)) = 1
013900040827     C                   MOVEL     '1'           §EMLERRO
014000140722     C                   IF        §EMLMSGO=*blanks
014100140722     C                   EVAL      §EMLMSGO='Indirizzo email formalmente errato'
014200060316     C                   ENDIF
014300040827     C                   ELSE
014400040827     C                   ENDIF
014500161019     C                   EVAL      §EMLINDO = sav§EMLINDO
014600040827     C                   ENDIF
014700040827     C*
014800140722     C                   EVAL      §EMLINDI = sav§EMLINDI
014900140722     C*
015000040827     C                   SETON                                        LR
015100001120     C*------------------------------------------------------------------------*
015200000411
015300001120
015400001120
015500001120
015600000414     C*------------------------------------------------------------------------*
015700000414     C* *INZSR - OPERAZIONI INIZIALI
015800000414     C*------------------------------------------------------------------------*
015900000414     C     *INZSR        BEGSR
016000001120     C*
016100000414     C*--------------------
016200000414     C* RICEVIMENTO PARAMETRI
016300000414     C*--------------------
016400000414     C     *ENTRY        PLIST
016500001120     C                   PARM                    DSEMAIL
016600140722     C*
016700140722     C* Salvo i parametri originali
016800140722     C                   EVAL      sav§EMLINDI = §EMLINDI
016900120514     C*
017000161019     C* Epuro subito dagli spazi inutili
017100161019     C                   EVAL      §EMLINDI = %scanrpl(' ':'':§EMLINDI)
017200001120     C*
017300000414     C                   ENDSR
