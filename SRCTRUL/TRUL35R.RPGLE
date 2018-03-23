000100020219     H DECEDIT('0,') DATEDIT(*DMY.)
000200020219
000300020219      ****************************************************************
000400020911      ** TRUL35R ** Calcolo del CHECK DIGIT (EDPFG)                 **
000500020219      ****************************************************************
000600020220
000700020911     FTRUL35D   CF   E             WORKSTN USROPN
000800020219
000900020219      *
001000020219      * SCHIERE
001100020911     D  SkChkDgt       S              1    DIM(50)
001200020221
001300020219      *
001400020219      * DS definite ESTERNAMENTE
001500020219      *
001600020911     D KPJBA         E ds
001700020221     D UT§DSE0F      E DS
001800020221
001900020221
002000020220      *
002100020221      * Status DS
002200020220      *
002300020220     D STATUS         SDS           333
002400020221     D  PROGR            *PROC
002500020220     D  PARMS            *PARMS
002600020220     D  USER                  81     90
002700020221
002800020219      *
002900020219      * VARIABILI
003000020219      *
003100020911     D lung            S              2  0 inz(*zeros)
003200020911     D WrkChkDgt       S              1  0 inz(*zeros)
003300020911     D WrkChkDgtNum    S              1  0 inz(*zeros)
003400020911     D WrkChkDgtAlfa   S              1    inz(*blanks)
003500020911     D SommaPari       S              5  0 inz(*zeros)
003600020911     D SommaDispari    S              5  0 inz(*zeros)
003700020911     D SommaTot        S              5  0 inz(*zeros)
003800020911     D Risultato       S              3  1 inz(*zeros)
003900020911     D I               S              2  0 inz(*zeros)
004000020911     D RestoDec        S              1  1 inz(*zeros)
004100020911     D Multiplo10      S              5  0 inz(*zeros)
004200020219
004300020221      ****************************************************************
004400020221
004500020911     C* Operazioni iniziali
004600020219     C                   exsr      OPEINI
004700020911     C*
004800020911do  1C                   do        *hival
004900020911     C*
005000020911     C* Emette videata
005100020911     C                   exsr      GESVID1
005200020911     C*
005300020221e   1C                   enddo
005400020911     C*
005500020911     C* Fine
005600020219     C                   exsr      ENDPGM
005700020911     C****************************************************************
005800020911
005900020911
006000020911     C****************************************************************
006100020911     C** OPEINI ** Operazioni Iniziali                              **
006200020911     C****************************************************************
006300020219     C     OPEINI        BEGSR
006400020911     C*
006500020221     C     *ENTRY        plist
006600020911     C                   parm                    KPJBA
006700020911     C*
006800020221     C                   z-add     1             codut
006900020221     C                   call      'X§PARUT'
007000020221     C                   parm                    UT§DSE0F
007100020221     C                   movel     RAGUT         RSUT
007200020911     C*
007300020219     C                   ENDSR
007400020911     C****************************************************************
007500020220
007600020911
007700020911
007800020911     C****************************************************************
007900020911     C** GESVID1    ** Gestione videata per richiesta parametri     **
008000020911     C****************************************************************
008100020221     C     GESVID1       BEGSR
008200020911     C*
008300020911if  1C                   if        not %open(TRUL35D)
008400020911     C                   open      TRUL35D
008500020221e   1C                   endif
008600020911     C*
008700020911     C                   clear                   TRUL35D1
008800020911     C                   exfmt     TRUL35D1
008900020911     C*
009000020911     C* F3/F12 = Fine
009100020221if  1C                   if        *inKC   or
009200020221     C                             *inKL
009300020220     C                   exsr      ENDPGM
009400020911     C                   else
009500020911     C*
009600020911     C* Esegue calcolo check-digits
009700020911     C                   exsr      CTRSTD
009800020911e   1C                   endIF
009900020911     C*
010000020220     C                   ENDSR
010100020911     C****************************************************************
010200020911
010300020911
010400020219
010500020911     C****************************************************************
010600020911     C** CTRSTD ** Controlli standard                               **
010700020911     C****************************************************************
010800020221     C     CTRSTD        BEGSR
010900020911     C*
011000020911     C* EAN   - Barcode secondo standerd EAN = EAN/UCC-13
011100020911     C     I35MOD        caseq     'EAN'         CtrEAN
011200020911     C*
011300020911     C* M1031 - Barcode generico Modulo 10
011400020911 xxx C*    I35MOD        caseq     'M10'         CtrM10
011500020911     C*
011600020911     C                   endcs
011700020911     C*
011800020911     C                   ENDSR
011900020911     C****************************************************************
012000020911
012100020911
012200020911
012300020911     C****************************************************************
012400020911     C** CTREAN ** Calcolo check-digits secondo standard EAN/UCC-13 **
012500020911     C****************************************************************
012600020911     C     CTREAN        BEGSR
012700020911     C*
012800020911     C                   z-add     *zeros        lung
012900020911     C                   eval      lung = %len(%trim(I35COD))
013000020911     C                   if        lung <> 12
013100020911     C                   eval      O35MSG = 'TRUL35R - Troppe cifre indicate'
013200020911     C                   else
013300020911     C*
013400020911     C                   CLEAR                   SkChkDgt
013500020911     C                   MOVEA     I35COD        SkChkDgt
013600020911     C* Inizializzo variabili di wrk
013700020911     C                   Z-ADD     *zeros        WrkChkDgt
013800020911     C                   Z-ADD     *zeros        WrkChkDgtNum
013900020911     C                   MOVEL     *blanks       WrkChkDgtAlfa
014000020911     C                   Z-ADD     *zeros        SommaPari
014100020911     C                   Z-ADD     *zeros        SommaDispari
014200020911     C                   Z-ADD     *zeros        SommaTot
014300020911     C                   Z-ADD     *zeros        Risultato
014400020911     C*
014500020911     C* Effettuo il calcolo
014600020911     C     1             DO        lung          I
014700020911     C                   IF        SkChkDgt(I) <> *blanks
014800020911     C* Stabilisco se trattasi di cifra in posizione pari o dispari
014900020911     C                   MOVEL(P)  SkChkDgt(I)   WrkChkDgt
015000020911     C                   EVAL      Risultato = I/2
015100020911     C                   Z-ADD     Risultato     RestoDec
015200020911     C                   IF        RestoDec = *zeros                            * è pari
015300020911     C                   ADD       WrkChkDgt     SommaPari
015400020911     C                   ELSE                                                   * è dispari
015500020911     C                   ADD       WrkChkDgt     SommaDispari
015600020911     C                   ENDIF
015700020911     C                   ENDIF
015800020911     C                   ENDDO
015900020911     C*
016000020911     C                   EVAL      SommaTot = (SommaDispari * 3) + SommaPari
016100020911     C                   Z-ADD     *zeros        Multiplo10
016200020911     C                   DOW       Multiplo10 < SommaTot
016300020911     C                   EVAL      Multiplo10 = Multiplo10 + 10
016400020911     C                   ENDDO
016500020911     C*
016600020911     C                   EVAL      WrkChkDgtNum = Multiplo10 - SommaTot
016700020911     C                   MOVEL     WrkChkDgtNum  WrkChkDgtAlfa
016800020911     C*
016900020911     C* Imposto i valori calcolati in output a video
017000020911     C                   eval      O35CKD = WrkChkDgtAlfa
017100020911     C                   eval      O35BRC = I35COD + WrkChkDgtAlfa
017200020911     C                   endif
017300020911     C*
017400020911     C                   ENDSR
017500020911     C****************************************************************
017600020911
017700020911
017800020911
017900020911     C****************************************************************
018000020911     C** ENDPGM    ** Uscita programma                             **
018100020911     C****************************************************************
018200020911     C     ENDPGM        BEGSR
018300020911     C*
018400020911     C                   seton                                        LR
018500020911     C*
018600020911     C                   ENDSR
018700020911     C****************************************************************
