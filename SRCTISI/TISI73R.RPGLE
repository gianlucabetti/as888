000100970610     h*--------------------------------------------------------------------------------------------*
000200971003     h* Allineamento anagrafico p.o. clienti - Esegue
000300970610     h*--------------------------------------------------------------------------------------------*
000400970610     h DECEDIT('0,') DATEDIT(*DMY.)
000500970610     f*--------------------------------------------------------------------------------------------*
000600970610     f* Data base
000700970610     f*--------------------------------------------------------------------------------------------*
000800970610     fazorg01l  IF   E           K DISK                                         Organigramma attuale
000900100119     fazcpf01l  Uf A E           k DISK                                         Organigramma vecchio
001000970610     d*--------------------------------------------------------------------------------------------*
001100970610     d* Data structure
001200970610     d*--------------------------------------------------------------------------------------------*
001300970610     d*----------------
001400970610     d* Variabili riferite al data base
001500970610     d*----------------
001600970610     d*----------------
001700970610     d* Variabili di controllo
001800970610     d*----------------
001900970610     d $fine           S              1    INZ('N')                             *fine programma
002000970610     d $recok          S              1    INZ('N')                             *validità record
002100970610     d $err            S              1    INZ('0')                             *errore generico
002200970610     d*----------------
002300970610     d* Variabili di programma
002400970610     d*----------------
002500971113     d a1              S              1                                         *alfanumerico
002600970610     d*----------------
002700970610     d* Ds
002800970610     d*----------------
002900970610     d kpjba         E DS                                                       *architettura
003000010921     d*----------------
003100010921     d dspar73         DS                                                       *ds parametri lancio
003200010921     d  p73xxx                        9                                          -filler
003300010921     d  p73azz                        1                                          -azzeramento linee
003400970610     c*--------------------------------------------------------------------------------------------*
003500970610     c* Main lines
003600970610     c*--------------------------------------------------------------------------------------------*
003700970610     C*
003800970610     C* posizionamento sul file e prima lettura
003900970703if  1c                   IF        $err='0'                                     *no errori
004000970610     C                   EXSR      setfil
004100970610     C*
004200970610     C* ciclo fino a fine file
004300970703do  2C                   DOW       $fine='N'
004400970610     C*
004500970610     C* scrive la filiale sul file di output
004600971003     C                   EXSR      scrcpf
004700970610     C*
004800970610     C* lettura sucessiva record
004900970610     C                   EXSR      letfil
005000970610     C*
005100970703e   2C                   ENDDO                                                  *fine filiali
005200010921     C*
005300100119     C                   CLEAR                   azcpf000
005400100119     C                   EVAL      cpffil = 789
005500100119     c     CPFfil        chain     AZCPF000
005600010921     C* se richiesto a fine riempimento da Organigramma aggiungo un record "fittizio"
005700010921     C                   IF        p73azz = 'S'
005800010921     C                   EVAL      cpfdes = 'DISK C'
005900100119     c                   if        %found(AZCPF01L)
006000100119     c                   clear                   AZCPF000
006100100119     c                   eval      CPFfil = 789
006200100119     c                   update    AZCPF000
006300100119     c                   else
006400010921     C                   WRITE     azcpf000
006500100119     c                   endif
006600100119     c                   Else
006700100119      * altrimento lo cancello (se già esistente)
006800100119     c                   if        %found(AZCPF01L)
006900100119     c                   DELETE    AZCPF000
007000100119     c                   endif
007100010921     C                   ENDIF
007200010921     C*
007300970703e   1C                   ENDIF
007400970610     C* fine pgm
007500970610     C                   SETON                                            lr
007600970610     c*--------------------------------------------------------------------------------------------*
007700970610     c* setfil - posizionamento sul file e prima lettura
007800970610     c*--------------------------------------------------------------------------------------------*
007900970610     C     setfil        BEGSR
008000970610     C*
008100970610     C* posizionamento sul file
008200970610     c     *loval        SETLL     azorg01l                           99
008300970610if  1c                   IF        NOT *in99
008400970610     c                   EXSR      letfil                                       *lettura record
008500970610x   1c                   ELSE
008600970610     C                   MOVEL     'S'           $fine                          *fine programma
008700970610e   1C                   ENDiF
008800970610     C*
008900970610     C                   ENDSR
009000970610     c*--------------------------------------------------------------------------------------------*
009100970610     c* letfil - lettura record
009200970610     c*--------------------------------------------------------------------------------------------*
009300970610     C     letfil        BEGSR
009400970610     C*
009500970610     C* Legge fino a:
009600970610     C*       -fine file
009700970610     C*       -trovato record valido
009800970610     c                   MOVEL     'N'           $recok                         *record non valido
009900970610do  1c                   DOW       NOT *in99 AND
010000970610     c                             $recok='N'
010100970611     C                   READ      azorg01l                               99
010200970610if  2c                   IF        *in99                                        *fine file
010300970610     c                   MOVEL     'S'           $fine                          *fine programma
010400970610x   2c                   ELSE                                                   *record trovato
010500970610     c                   EXSR      chkrec                                       *controlla record
010600970610e   2c                   ENDIF
010700970610e   1c                   ENDDO
010800970610     C*
010900970610     C                   ENDSR
011000970610     c*--------------------------------------------------------------------------------------------*
011100970610     c* chkrec - controlla validità record
011200970610     c*--------------------------------------------------------------------------------------------*
011300970610     C     chkrec        BEGSR
011400970610     C*
011500970610     c                   MOVEL     'S'           $recok                         *record valido
011600970610     C*
011700970610     C* filiale annullata, esclude
011800970610if  1c                   IF        orgfva<>*blanks
011900970610     c                   MOVEL     'N'           $recok                         *record non valido
012000970610e   1c                   ENDIF
012100970610     C*
012200970610     C* filiale varia, esclude
012300970610if  1c                   IF        orgfag<>'F' AND
012400970610     c                             orgfag<>'A'
012500970610     c                   MOVEL     'N'           $recok                         *record non valido
012600970610e   1c                   ENDIF
012700970610     C*
012800970610     C                   ENDSR
012900970610     c*--------------------------------------------------------------------------------------------*
013000971003     C* scrcpf  - scrive la filiale sul file di output
013100970610     c*--------------------------------------------------------------------------------------------*
013200971003     C     scrcpf        BEGSR
013300100119      *
013400100119     c     ORGfil        chain     AZCPF000
013500970610     C*
013600970610     C* imposta i campi del record da scrivere
013700971003     C                   Z-ADD     orgfil        cpffil
013800970611if  1c                   IF        orgde5<>*blanks
013900971003     C                   MOVEL(P)  orgde5        cpfdes
014000970611x   1c                   ELSE
014100971003     C                   MOVEL(P)  orgdes        cpfdes
014200970611e   1c                   ENDIF
014300971003     C                   MOVEL     orgind        cpfind
014400971003     C                   MOVEL     orgcpf        cpfcap
014500971003     C                   MOVEL     orgloc        cpfloc
014600971003     C                   MOVEL     orgpro        cpfpro
014700971003     C                   MOVEL     *blanks       cpfnaz
014800971003     C                   MOVEL     orgtel        cpftel
014900971003     C                   MOVEL     orgfax        cpffax
015000971113     c                   MOVE      orgfax        a1
015100971113if  1c                   IF        a1 = *blanks
015200971113     c                   MOVE      '.'           cpffax
015300971113e   1c                   ENDIF
015400970610     C*
015500970610     C* scrive il record
015600100119     c                   if        %found(AZCPF01L)
015700100119     c                   update    AZCPF000
015800100119     c                   else
015900971003     C                   WRITE     azcpf000
016000100119     c                   endif
016100970610     C*
016200970610     C                   ENDSR
016300970610     c*--------------------------------------------------------------------------------------------*
016400970610     C* *inzsr - operazioni iniziali
016500970610     c*--------------------------------------------------------------------------------------------*
016600970610     C     *inzsr        BEGSR
016700970610     c*
016800970610     c* ricevimento parametri
016900970610     c     *ENTRY        PLIST
017000970610     c                   PARM                    kpjba
017100010921     c                   MOVEL     kpjbu         dspar73
017200970610     C*
017300970610     C                   ENDSR
