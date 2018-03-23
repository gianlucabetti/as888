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
001100040827     D SimbolOK        C                   CONST('_-.§@&;')
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
003100140722     D SkSplitCSV_4    S            128    DIM(512)
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
004400040827     C* Inizializzo flag e messaggio d errore
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
005700040827     C*
005800040827     C* Effettuo controllo su l'elenco completo dei caratteri validi in un indirizzo e-mail
005900040827     C                   EVAL      CharOK = NumOK + MinuOK + MaiuOK + SimbolOK
006000040827     C                   EVAL      WstrIND = §EMLINDI
006100040827     C                   Z-ADD     1             Indice
006200040827     C                   DOW       Indice<=%len(%trim(WstrIND))
006300040827     C                   EVAL      SingleChar=%subst(WstrIND:Indice:1)
006400040827     C     SingleChar    SCAN      CharOK
006500040827     C                   IF        (NOT %found)
006600040827     C                   MOVEL     '1'           §EMLERRO
006700040827     C                   EVAL      §EMLMSGO = 'E-mail errato: ' +
006800040827     C                                      'presenti caratteri non validi '+
006900040827     C                                      '('+ SingleChar + ')'
007000040827     C                   LEAVE
007100040827     C                   ENDIF
007200040827     C                   ADD       1             Indice
007300040827     C                   ENDDO
007400040827     C*
007500040827     C* Effettuo controlli su correttezza formale singoli indirizzi e-mail
007600040827     C                   IF        §EMLERRO = *blanks
007700040827     C                   EVAL      EmlMail = %trim(§EMLINDI)
007800040827     C                   EVAL      EmlPos = *zeros
007900040827     C                   EVAL      EmlPosDa = 1
008000040827     C* Innanzitutto verifico che nn inizi con il carattere separatore indirizzi e-mail
008100040827     C                   DOW       %subst(EmlMail:1:1) = ';'
008200040827     C                   EVAL      %subst(EmlMail:1:1) = *blanks
008300040827     C                   EVAL      EmlMail = %trim(EmlMail)
008400040827     C                   ENDDO
008500040827     C* Poi verifico che nn finisca con il carattere separatore indirizzi e-mail
008600040827     C                   DOW       %subst(EmlMail:%len(%trim(EmlMail)):1) = ';'
008700040827     C                   EVAL      %subst(EmlMail:%len(%trim(EmlMail)):1)=*blanks
008800040827     C                   EVAL      EmlMail = %trim(EmlMail)
008900040827     C                   ENDDO
009000140722     C*
009100140722     C* Quindi chiamo il driver di controllo formale indirizzo email per ogni indirizzo email in inp
009200140722     C                   eval      InStringa = %trim(EmlMail)
009300140722     C*
009400140722     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
009500140722     C                   CALL      'XSPLIT2'
009600140722     C                   PARM                    InStringa
009700140722     C                   PARM      ';'           InSepara
009800140722     C                   PARM      '4'           InTypeOut
009900140722     C                   PARM      ''            wSkParam
010000140722     C                   PARM                    OutErrore
010100140722     C                   PARM                    OutMsg
010200140722     C                   MOVEA     wSkParam      SkSplitCSV_4
010300140722     C*
010400140722     C* Valorizzo la schiera delle intestazioni colonne (elementi)
010500140722     C                   z-add     1             x                 4 0
010600140722     C                   dow       x <= %elem(SkSplitCSV_4)
010700140722     C                   if        SkSplitCSV_4(x) = *blanks
010800140722     C                   leave
010900140722     C                   else
011000140722     C                   clear                   DSEMAIL
011100140722     C                   eval      §EMLINDI = %trim(SkSplitCSV_4(x))
011200140722     C                   call      'XEMAIL1'
011300140722     C                   parm                    DSEMAIL
011400140722     C                   if        §EMLERRO = '1'
011500140722     C                   movel     'N'           FlgOK
011600140722     C                   leave
011700140722     C                   else
011800140722     C                   if        x > 1
011900140722     C                   eval      sav§EMLINDO = %trim(sav§EMLINDO) + ';' +
012000140722     C                                           %trim(§EMLINDO)
012100140722     C                   else
012200140722     C                   eval      sav§EMLINDO = %trim(§EMLINDO)
012300140722     C                   endif
012400140722     C                   endif
012500140722     C                   endif
012600140722     C                   add       1             x
012700140722     C                   enddo
012800041215     C*
012900041215     C                   ENDIF
013000041215     C*
013100041215     C* Al termine se rilevati errori emetto messaggio relativo
013200040827     C                   IF        FlgOk = 'N' OR
013300040827     C                             %len(%trim(EmlMail)) = 1
013400040827     C                   MOVEL     '1'           §EMLERRO
013500140722     C                   IF        §EMLMSGO=*blanks
013600140722     C                   EVAL      §EMLMSGO='Indirizzo email formalmente errato'
013700060316     C                   ENDIF
013800040827     C                   ELSE
013900140722     C                   EVAL      §EMLINDO = sav§EMLINDO
014000040827     C                   ENDIF
014100040827     C                   ENDIF
014200040827     C*
014300140722     C                   EVAL      §EMLINDI = sav§EMLINDI
014400140722     C*
014500040827     C                   SETON                                        LR
014600001120     C*------------------------------------------------------------------------*
014700000411
014800001120
014900001120
015000001120
015100000414     C*------------------------------------------------------------------------*
015200000414     C* *INZSR - OPERAZIONI INIZIALI
015300000414     C*------------------------------------------------------------------------*
015400000414     C     *INZSR        BEGSR
015500001120     C*
015600000414     C*--------------------
015700000414     C* RICEVIMENTO PARAMETRI
015800000414     C*--------------------
015900000414     C     *ENTRY        PLIST
016000001120     C                   PARM                    DSEMAIL
016100140722     C*
016200140722     C* Salvo i parametri originali
016300140722     C                   EVAL      sav§EMLINDI = §EMLINDI
016400120514     C*
016500140722     C***/EXEC SQL
016600140722     C***+ SET :§EMLINDI = REPLACE(:§EMLINDI, ' ' , '')
016700140722     C***/END-EXEC
016800001120     C*
016900000414     C                   ENDSR
