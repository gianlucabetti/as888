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
001800001120
001900001120     D*-----------------
002000001120     D* DS DI PROCEDURA
002100001120     D*-----------------
002200001120     D DSEMAIL       E DS
002300001120
002400000411
002500000414     C*------------------------------------------------------------------------*
002600000414     C* MAIN LINE
002700000414     C*------------------------------------------------------------------------*
002800040827     C*
002900040827     C* Inizializzo flag e messaggio d errore
003000040827     C                   MOVEL     *blanks       §EMLERRO
003100040827     C                   MOVEL     *blanks       §EMLMSGO
003200040827     C*
003300040827     C* Inizializzo flag d erorre generale (serve in caso d controllo <n> indirizzi e-mail)
003400040827     C                   MOVEL     'S'           FlgOK             1
003500041215     C*
003600041215     C* Quindi verifico subito se il parametro contenente la stringa da verificare è valorizzato
003700041215     C                   IF        §EMLINDI = *blanks
003800041215     C                   MOVEL     'N'           FlgOK
003900041215     C                   EVAL      §EMLMSGO = 'E-mail errato: ' +
004000041215     C                                      'indirizzo non valorizzato'
004100041215     C                   ELSE
004200040827     C*
004300040827     C* Effettuo controllo su l'elenco completo dei caratteri validi in un indirizzo e-mail
004400040827     C                   EVAL      CharOK = NumOK + MinuOK + MaiuOK + SimbolOK
004500040827     C                   EVAL      WstrIND = §EMLINDI
004600040827     C                   Z-ADD     1             Indice
004700040827     C                   DOW       Indice<=%len(%trim(WstrIND))
004800040827     C                   EVAL      SingleChar=%subst(WstrIND:Indice:1)
004900040827     C     SingleChar    SCAN      CharOK
005000040827     C                   IF        (NOT %found)
005100040827     C                   MOVEL     '1'           §EMLERRO
005200040827     C                   EVAL      §EMLMSGO = 'E-mail errato: ' +
005300040827     C                                      'presenti caratteri non validi '+
005400040827     C                                      '('+ SingleChar + ')'
005500040827     C                   LEAVE
005600040827     C                   ENDIF
005700040827     C                   ADD       1             Indice
005800040827     C                   ENDDO
005900040827     C*
006000040827     C* Effettuo controlli su correttezza formale singoli indirizzi e-mail
006100040827     C                   IF        §EMLERRO = *blanks
006200040827     C                   EVAL      EmlMail = %trim(§EMLINDI)
006300040827     C                   EVAL      EmlPos = *zeros
006400040827     C                   EVAL      EmlPosDa = 1
006500040827     C* Innanzitutto verifico che nn inizi con il carattere separatore indirizzi e-mail
006600040827     C                   DOW       %subst(EmlMail:1:1) = ';'
006700040827     C                   EVAL      %subst(EmlMail:1:1) = *blanks
006800040827     C                   EVAL      EmlMail = %trim(EmlMail)
006900040827     C                   ENDDO
007000040827     C* Poi verifico che nn finisca con il carattere separatore indirizzi e-mail
007100040827     C                   DOW       %subst(EmlMail:%len(%trim(EmlMail)):1) = ';'
007200040827     C                   EVAL      %subst(EmlMail:%len(%trim(EmlMail)):1)=*blanks
007300040827     C                   EVAL      EmlMail = %trim(EmlMail)
007400040827     C                   ENDDO
007500040929     C* Poi verifico che nn vi siano spazi nel mezzo
007600040929     C                   Z-ADD     1             i                 3 0
007700041215     C                   EVAL      i = %scan(';':EmlMail:i)
007800041215     C                   IF        i > *zeros
007900041215     C                   EVAL      i = i + 1
008000041215     C                   ENDIF
008100041215     C                   DOW       i < %len(%trim(EmlMail)) AND i > *zeros
008200041215     C                   IF        %subst(EmlMail:i:1)=*blanks
008300040929     C                   EVAL      EmlMail=%subst(EmlMail:1:i-1)+
008400040929     C                                     %subst(EmlMail:i+1)
008500040929     C                   EVAL      i = i - 1
008600041215     C                   ELSE
008700041215     C                   EVAL      i = %scan(';':EmlMail:i)
008800041215     C                   IF        i > *zeros
008900120514     C*// lo fa sotto:   EVAL      i = i + 1
009000041215     C                   ELSE
009100041215     C                   LEAVE
009200041215     C                   ENDIF
009300040929     C                   ENDIF
009400040929     C                   ADD       1             i
009500040929     C                   ENDDO
009600040827     C*
009700100614     C                   DOW       EmlPosDa <= %len(%trim(EmlMail))
009800040827     C                   CLEAR                   DSEMAIL
009900040827     C                   EVAL      EmlPos = %scan(';':EmlMail:EmlPosDa)
010000040827     C                   IF        EmlPos > *zeros
010100040827     C* Valorizzo la DS d procedura utilità controllo formale indirizzi e-mail
010200040827     C                   EVAL      §EMLINDI = %subst(EmlMail:EmlPosDa
010300040827     C                             :(EmlPos-EmlPosDa))
010400040827     C                   ELSE
010500040827     C* Valorizzo la DS d procedura utilità controllo formale indirizzi e-mail
010600040827     C                   EVAL      §EMLINDI = %subst(EmlMail:EmlPosDa)
010700040827     C                   ENDIF
010800040827     C* Se c'è 1 indirizzo da controllare chiamo il *pgm d utilità controllo formale indirizzi e-mail
010900040827     C                   IF        §EMLINDI <> *blanks
011000040827     C                   CALL      'XEMAIL1'
011100040827     C                   PARM                    DSEMAIL
011200040827     C                   IF        §EMLERRO = '1'
011300040827     C                   MOVEL     'N'           FlgOK
011400040827     C                   LEAVE
011500040827     C                   ELSE
011600040827     C                   EVAL      %subst(EmlMail:EmlPosDa:%len(%trim(§EMLINDO))
011700040827     C                             )=%trim(§EMLINDO)
011800040827     C                   ENDIF
011900040827     C* Salvo la nuova posizione d partenza ricerca separatore indirizzi e-mail
012000040827     C                   EVAL      EmlPosDa = EmlPos
012100040827     C                   ENDIF
012200040827     C* Se nn trovati + separatori indirizzi e-mail esco dal ciclo
012300040827     C                   IF        EmlPos = *zeros
012400040827     C                   LEAVE
012500040827     C                   ENDIF
012600040827     C* Incremento d 1 byte la posizione iniziale da cui effettuare la ricerca del separatore indirizzi e-mail
012700040827     C                   EVAL      EmlPosDa = EmlPosDa + 1
012800040827     C                   ENDDO
012900041215     C*
013000041215     C                   ENDIF
013100041215     C*
013200041215     C* Al termine se rilevati errori emetto messaggio relativo
013300040827     C                   IF        FlgOk = 'N' OR
013400040827     C                             %len(%trim(EmlMail)) = 1
013500040827     C                   MOVEL     '1'           §EMLERRO
013600060316     C                   IF        §EMLMSGO=*blanks
013700040827     C                   EVAL      §EMLMSGO='Indirizzo email formalmente errato'
013800060316     C                   ENDIF
013900040827     C                   ELSE
014000040827     C                   EVAL      §EMLINDO=EmlMail
014100040827     C                   ENDIF
014200040827     C                   ENDIF
014300040827     C*
014400040827     C                   SETON                                        LR
014500001120     C*------------------------------------------------------------------------*
014600000411
014700001120
014800001120
014900001120
015000000414     C*------------------------------------------------------------------------*
015100000414     C* *INZSR - OPERAZIONI INIZIALI
015200000414     C*------------------------------------------------------------------------*
015300000414     C     *INZSR        BEGSR
015400001120     C*
015500000414     C*--------------------
015600000414     C* RICEVIMENTO PARAMETRI
015700000414     C*--------------------
015800000414     C     *ENTRY        PLIST
015900001120     C                   PARM                    DSEMAIL
016000120514     C*
016100120514     C/EXEC SQL
016200120514     C+ SET :§EMLINDI = REPLACE(:§EMLINDI, ' ' , '')
016300120514     C/END-EXEC
016400001120     C*
016500000414     C                   ENDSR
