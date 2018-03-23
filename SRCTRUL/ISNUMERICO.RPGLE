000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400000524     D WrkStr          S             30
000500000524     D FinStr          S             30    INZ(*all'0')
000600000414     D LenWrkStr       S              2  0 INZ(0)
000700000524     D LenFin          S              2  0 INZ(0)
000800000414     D SingleChar      S              1
000900000414     D Indice          S              2  0 INZ(1)
001000000414     D ElencoNum       S             10    INZ('1234567890')
001100000414     D DecSepPos       S              2  0 INZ(0)
001200011213     D MilSepPos       S              2  0 INZ(0)
001300000414     D WrkDec          S              9    VARYING
001400000414     D                                     INZ(*all'0')
001500000427     D Segno           S              1  0 INZ(1)
001600011213     D StrMilChr       S              1    INZ
001700041210     D wContaDecChr    S              3  0 INZ(*zeros)
001800000411
001900000414     C*------------------------------------------------------------------------*
002000000414     C* MAIN LINE
002100000414     C*------------------------------------------------------------------------*
002200000414     C*
002300001108     C* Prima di tutto verifico che il parametro da "convertire" non sia vuoto
002400001108     C                   IF        PiStr <> *blanks
002500000414     C                   EVAL      PiNum='1'
002600000427     C* Tolgo gli spazi a sinistra e a destra
002700000414     C                   EVAL      WrkStr=%trim(PiStr)
002800010612     C* Se la stringa è nel formato ',2' => la considero '0,2'
002900010612     C                   IF        %subst(WrkStr:1:1) = PiDecChr
003000010612     C                   EVAL      WrkStr = '0' + %trim(WrkStr)
003100010612     C                   ENDIF
003200000427     C* Reperisco lunghezza del contenuto della stringa "spurgato" dagli spazi
003300000524     C                   EVAL      LenWrkStr=%len(%trim(WrkStr))
003400000427     C* Verifico se specificato (a sin. o des. della stringa) il segno
003500000427     C                   IF        %subst(WrkStr:1:1) = '-' OR
003600000427     C                             %subst(WrkStr:LenWrkStr:1) = '-'
003700000427     C                   EVAL      Segno = -1
003800000427     C                   ENDIF
003900011213     C*
004000011213     C* Se il campo è editato con il separatore delle migliaia => elimino il carattere di editazione
004100011213     C                   EVAL      Indice = 1
004200011213     C                   DOW       Indice<=LenWrkStr
004300011213     C                   EVAL      SingleChar=%subst(WrkStr:Indice:1)
004400011213     C                   IF        SingleChar=StrMilChr
004500011213     C                   EVAL      WrkStr=%subst(WrkStr:1:Indice-1) +
004600011213     C                                    %subst(WrkStr:Indice+1)
004700011213     C                   ENDIF
004800011213     C                   ADD       1             Indice
004900011213     C                   ENDDO
005000011213     C* Ri-aggiorno la lunghezza della stringa numerica senza i caratteri di editazione
005100011213     C                   EVAL      LenWrkStr=%len(%trim(WrkStr))
005200041210     C*
005300041210     C* Inizializzo flag che indica la presenza del carattere d separatore decimale
005400041210     C                   Z-ADD     *zeros        wContaDecChr
005500011213     C*
005600000427     C* Determino se trattasi di stringa valida ai fini numerici
005700011213     C                   EVAL      Indice = 1
005800000414     C                   DOW       Indice<=LenWrkStr
005900000414     C                   EVAL      SingleChar=%subst(WrkStr:Indice:1)
006000000414     C     SingleChar    SCAN      ElencoNum
006100000414     C                   IF        (NOT %found) AND
006200041210     C                             SingleChar<>PiDecChr
006300000414     C                   EVAL      PiInt='0'
006400000414     C                   EVAL      PiNum='0'
006500041210     C                   LEAVE
006600041210     C                   ELSE
006700041210     C                   IF        SingleChar=PiDecChr
006800041210     C                   IF        wContaDecChr=*zeros
006900041210     C                   ADD       1             wContaDecChr
007000041210     C                   ELSE
007100041210     C                   EVAL      PiInt='0'
007200041210     C                   EVAL      PiNum='0'
007300041210     C                   LEAVE
007400041210     C                   ENDIF
007500041210     C                   ENDIF
007600000414     C                   ENDIF
007700000414     C                   ADD       1             Indice
007800000414     C                   ENDDO
007900000414
008000000427     C* In caso di stringa valida a fini numerici verifico se trattasi di
008100000427     C* numero intero o decimale (il carattere di separazione decimale è
008200000427     C* parametrizzato in input: o "," o ".")
008300000414     C                   IF        PiNum='1'
008400000414     C     PiDecChr      SCAN      WrkStr        DecSepPos
008500020305     C                   IF        %found
008600020305     C* Verifico che nn vi siano + di 9 cifre decimali
008700020305     C                   IF        LenWrkStr-DecSepPos > 9
008800020312     C                   EVAL      %subst(WrkStr:(DecSepPos+9+1))
008900020312     C                             =*blanks
009000020312     C                   EVAL      LenWrkStr=%len(%trim(WrkStr))
009100020312     C                   EVAL      %len(WrkDec)=0
009200020305     C                   ELSE
009300020312     C                   EVAL      %len(WrkDec)=(9-(LenWrkStr-DecSepPos))
009400020305     C                   ENDIF
009500020305     C*
009600020312     C* Verifico che qualora esista il segno di separatore decimali i decimali nn siano tutti a zero,
009700020312     C* altrimenti lo considero comunque un intero
009800020312     C                   IF        %subst(WrkStr:(DecSepPos+1):(%len(%trim
009900020312     C                             (WrkStr))-DecSepPos))=*zeros
010000020312     C                   EVAL      PiInt='1'
010100020312     C                   ENDIF
010200020312     C*
010300020312     C                   EVAL      WrkStr=%subst(WrkStr:1:(DecSepPos-1)) +
010400000524     C                             %subst(WrkStr:(DecSepPos+1):
010500000524     C                             (LenWrkStr-DecSepPos)) +
010600000524     C                             WrkDec
010700000524     C                   EVAL      LenFin=%len(%trim(WrkStr))
010800000524     C                   EVAL      %subst(FinStr:(30-LenFin+1):LenFin)=
010900000524     C                             %trim(WrkStr)
011000020312     C                   ELSE
011100000414     C                   EVAL      PiInt='1'
011200020312     C                   EVAL      WrkStr=%trim(WrkStr) + WrkDec
011300000524     C                   EVAL      LenFin=%len(%trim(WrkStr))
011400000524     C                   EVAL      %subst(FinStr:(30-LenFin+1):LenFin)=
011500000524     C                             %trim(WrkStr)
011600000414     C                   ENDIF
011700000524     C                   MOVE      FinStr        PiVal
011800000414     C                   ENDIF
011900000414     C*
012000041210     C* Applico il segno algebrico positivo o negativo (ma solo se il valore reperito è <> 0)
012100041210     C                   IF        PiVal <> *zeros
012200000427     C                   EVAL      PiVal=Pival*Segno
012300001108     C                   ENDIF
012400041210     C                   ENDIF
012500000427     C*
012600000411
012700000414     C*
012800000414     C                   SETON                                        LR
012900000411
013000000414     C*------------------------------------------------------------------------*
013100000414     C* *INZSR - OPERAZIONI INIZIALI
013200000414     C*------------------------------------------------------------------------*
013300000414     C     *INZSR        BEGSR
013400000414     C*--------------------
013500000414     C* RICEVIMENTO PARAMETRI
013600000414     C*--------------------
013700000414     C     *ENTRY        PLIST
013800000414     C                   PARM                    PiStr            30
013900000414     C                   PARM                    PiDecChr          1
014000000414     C                   PARM                    PiVal            30 9
014100000414     C                   PARM                    PiInt             1
014200000414     C                   PARM                    PiNum             1
014300041210     C*
014400041210     C* Inizializzo subito i parametri d ritorno
014500041210     C                   Z-ADD     *zeros        PiVal
014600041210     C                   MOVEL     '0'           PiInt
014700041210     C                   MOVEL     '0'           PiNum
014800000414     C*
014900011213     C* Detremino l'eventuale carattere di editazione numerica: se la , divide i decimali
015000011213     C* dalla parte intera allora il . sarà il separatore delle migliaia; altrimenti viceversa
015100011213     C                   If        PiDecChr = ','
015200011213     C                   eval      StrMilChr = '.'
015300011213     C                   else
015400011213     C                   eval      StrMilChr = ','
015500011213     C                   endif
015600011213     C*
015700000414     C                   ENDSR
