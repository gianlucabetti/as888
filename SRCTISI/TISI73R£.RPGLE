000100970610     h*--------------------------------------------------------------------------------------------*
000200971003     h* Allineamento anagrafico p.o. clienti - Esegue
000300970610     h*--------------------------------------------------------------------------------------------*
000400970610     h DECEDIT('0,') DATEDIT(*DMY.)
000500970610     f*--------------------------------------------------------------------------------------------*
000600970610     f* Data base
000700970610     f*--------------------------------------------------------------------------------------------*
000800970610     fazorg01l  IF   E           K DISK                                         Organigramma attuale
000900971003     fazcpf00f  O  A E             DISK                                         Organigramma vecchio
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
005300010921     C* se richiesto a fine riempimento da Organigramma aggiungo un record "fittizio"
005400010921     C                   IF        p73azz = 'S'
005500010921     C                   CLEAR                   azcpf000
005600010921     C                   EVAL      cpffil = 789
005700010921     C                   EVAL      cpfdes = 'DISK C'
005800010921     C                   WRITE     azcpf000
005900010921     C                   ENDIF
006000010921     C*
006100970703e   1C                   ENDIF
006200970610     C* fine pgm
006300970610     C                   SETON                                            lr
006400970610     c*--------------------------------------------------------------------------------------------*
006500970610     c* setfil - posizionamento sul file e prima lettura
006600970610     c*--------------------------------------------------------------------------------------------*
006700970610     C     setfil        BEGSR
006800970610     C*
006900970610     C* posizionamento sul file
007000970610     c     *loval        SETLL     azorg01l                           99
007100970610if  1c                   IF        NOT *in99
007200970610     c                   EXSR      letfil                                       *lettura record
007300970610x   1c                   ELSE
007400970610     C                   MOVEL     'S'           $fine                          *fine programma
007500970610e   1C                   ENDiF
007600970610     C*
007700970610     C                   ENDSR
007800970610     c*--------------------------------------------------------------------------------------------*
007900970610     c* letfil - lettura record
008000970610     c*--------------------------------------------------------------------------------------------*
008100970610     C     letfil        BEGSR
008200970610     C*
008300970610     C* Legge fino a:
008400970610     C*       -fine file
008500970610     C*       -trovato record valido
008600970610     c                   MOVEL     'N'           $recok                         *record non valido
008700970610do  1c                   DOW       NOT *in99 AND
008800970610     c                             $recok='N'
008900970611     C                   READ      azorg01l                               99
009000970610if  2c                   IF        *in99                                        *fine file
009100970610     c                   MOVEL     'S'           $fine                          *fine programma
009200970610x   2c                   ELSE                                                   *record trovato
009300970610     c                   EXSR      chkrec                                       *controlla record
009400970610e   2c                   ENDIF
009500970610e   1c                   ENDDO
009600970610     C*
009700970610     C                   ENDSR
009800970610     c*--------------------------------------------------------------------------------------------*
009900970610     c* chkrec - controlla validità record
010000970610     c*--------------------------------------------------------------------------------------------*
010100970610     C     chkrec        BEGSR
010200970610     C*
010300970610     c                   MOVEL     'S'           $recok                         *record valido
010400970610     C*
010500970610     C* filiale annullata, esclude
010600970610if  1c                   IF        orgfva<>*blanks
010700970610     c                   MOVEL     'N'           $recok                         *record non valido
010800970610e   1c                   ENDIF
010900970610     C*
011000970610     C* filiale varia, esclude
011100970610if  1c                   IF        orgfag<>'F' AND
011200970610     c                             orgfag<>'A'
011300970610     c                   MOVEL     'N'           $recok                         *record non valido
011400970610e   1c                   ENDIF
011500970610     C*
011600970610     C                   ENDSR
011700970610     c*--------------------------------------------------------------------------------------------*
011800971003     C* scrcpf  - scrive la filiale sul file di output
011900970610     c*--------------------------------------------------------------------------------------------*
012000971003     C     scrcpf        BEGSR
012100970610     C*
012200970610     C* imposta i campi del record da scrivere
012300971003     C                   Z-ADD     orgfil        cpffil
012400970611if  1c                   IF        orgde5<>*blanks
012500971003     C                   MOVEL(P)  orgde5        cpfdes
012600970611x   1c                   ELSE
012700971003     C                   MOVEL(P)  orgdes        cpfdes
012800970611e   1c                   ENDIF
012900971003     C                   MOVEL     orgind        cpfind
013000971003     C                   MOVEL     orgcpf        cpfcap
013100971003     C                   MOVEL     orgloc        cpfloc
013200971003     C                   MOVEL     orgpro        cpfpro
013300971003     C                   MOVEL     *blanks       cpfnaz
013400971003     C                   MOVEL     orgtel        cpftel
013500971003     C                   MOVEL     orgfax        cpffax
013600971113     c                   MOVE      orgfax        a1
013700971113if  1c                   IF        a1 = *blanks
013800971113     c                   MOVE      '.'           cpffax
013900971113e   1c                   ENDIF
014000970610     C*
014100970610     C* scrive il record
014200971003     C                   WRITE     azcpf000
014300970610     C*
014400970610     C                   ENDSR
014500970610     c*--------------------------------------------------------------------------------------------*
014600970610     C* *inzsr - operazioni iniziali
014700970610     c*--------------------------------------------------------------------------------------------*
014800970610     C     *inzsr        BEGSR
014900970610     c*
015000970610     c* ricevimento parametri
015100970610     c     *ENTRY        PLIST
015200970610     c                   PARM                    kpjba
015300010921     c                   MOVEL     kpjbu         dspar73
015400970610     C*
015500970610     C                   ENDSR
