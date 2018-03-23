000100970610     h*--------------------------------------------------------------------------------------------*
000200970610     h* Allineamento organigramma clienti - Esegue
000300970610     h*--------------------------------------------------------------------------------------------*
000400970610     h DECEDIT('0,') DATEDIT(*DMY.)
000500970610     f*--------------------------------------------------------------------------------------------*
000600970610     f* Data base
000700970610     f*--------------------------------------------------------------------------------------------*
000800970610     fazorg01l  IF   E           K DISK                                         Organigramma attuale
000900970610     fazfil00f  O  A E             DISK                                         Organigramma vecchio
001000970610     f                                     RENAME(azfil:azfil0)
001100970610     f                                     PREFIX(a_)
001200970610     fazfil10f  O  A E             DISK                                         organigramma vecchio
001300970610     f                                     RENAME(azfil:azfil1)
001400970610     f                                     PREFIX(b_)
001500970610     d*--------------------------------------------------------------------------------------------*
001600970610     d* Data structure
001700970610     d*--------------------------------------------------------------------------------------------*
001800970610     d*----------------
001900970610     d* Variabili riferite al data base
002000970610     d*----------------
002100970610     d*----------------
002200970610     d* Variabili di controllo
002300970610     d*----------------
002400970610     d $fine           S              1    INZ('N')                             *fine programma
002500970610     d $recok          S              1    INZ('N')                             *validità record
002600970610     d $err            S              1    INZ('0')                             *errore generico
002700970610     d*----------------
002800970610     d* Variabili di programma
002900970610     d*----------------
003000970610     d wn14            S             14  0                                      *Numerico 14
003100970610     d wn8             S              8  0                                      *Numerico 8
003200970610     d wa1             S              1                                         *Alfanumerico 1
003300970610     d wa5             S              5                                         *Alfanumerico 5
003400971020     d datcor          S              8  0                                      *data corrente
003500970610     d*----------------
003600970610     d* Ds
003700970610     d*----------------
003800971020     d wlbda8          DS                  INZ                                  *controlla data
003900971020     d  g08dat                        8  0                                       -data dritta
004000971020     d  g08inv                        8  0                                       -data invertita
004100971020     d  g08err                        1                                          -errore
004200971020     d  g08tgi                        5  0                                       -giorni fra date
004300970707     d p96dri          S              8  0                                      TISI96R-data riferim
004400970707     d p96ver          S              5  0                                      TISI96R-versione
004500970610     d dspar50         DS                                                       *ds parametri lancio
004600971002     d  p50dri                        8                                          -data riferimento
004700971002     d  p50fil                        3                                          -filiale
004800970610     d  p50fmr                        1                                          -tracciato record
004900970610     d  p50sac                        1                                          -solo agg.cappario
005000010921     d  p50bos                        1                                          -clienti BAR o SDI
005100010921     d  p50azz                        1                                          -azzeramento linee
005200970618     d ut§dse        E DS                  EXTNAME(ut§dse0f)                    *reper.dati utente
005300970618     d cncr80        E DS
005400971020     d dslv55        E DS                  EXTNAME(fnlv55ds)                    *recupera terminal
005500970610     d kpjba         E DS                                                       *architettura
005600970610     c*--------------------------------------------------------------------------------------------*
005700970610     c* Main lines
005800970610     c*--------------------------------------------------------------------------------------------*
005900970618     C*
006000970618     C* Imposta le variabili di lavoro in base ai parametri di entrata
006100970618     C                   EXSR      imppar
006200970610     C*
006300970610     C* posizionamento sul file e prima lettura
006400970703if  1c                   IF        $err='0'                                     *no errori
006500970610     C                   EXSR      setfil
006600970610     C*
006700970610     C* ciclo fino a fine file
006800970703do  2C                   DOW       $fine='N'
006900970610     C*
007000971020     C* recupera i terminal della filiale
007100971020     C                   EXSR      rectfx
007200970610     C*
007300970610     C* scrive la filiale sul file di output
007400970610     C                   EXSR      scrfil0
007500970610     C*
007600970610     C* scrive la filiale sul file di output
007700970610     C                   EXSR      scrfil1
007800970610     C*
007900970610     C* lettura sucessiva record
008000970610     C                   EXSR      letfil
008100970610     C*
008200970703e   2C                   ENDDO                                                  *fine filiali
008300010921     C*
008400010921     C* se richiesto azzeramento linee aggiungo in entrambi i file un record "fittizio"
008500010921     C                   IF        p50azz = 'S'
008600010921     C* 1° file
008700010921     C                   CLEAR                   azfil0
008800010921     C                   EVAL      a_filfil = 789
008900010921     C                   EVAL      a_fildes = 'DISK C'
009000010921     C                   EVAL      a_filtrm = 789
009100010921     C                   WRITE     azfil0
009200010921     C* 2° file
009300010921     C                   CLEAR                   azfil1
009400010921     C                   EVAL      b_filfil = '789'
009500010921     C                   EVAL      b_fildes = 'DISK C'
009600010921     C                   EVAL      b_filtrm = '789'
009700010921     C                   WRITE     azfil1
009800010921     C*
009900010921     C                   ENDIF
010000010921     C*
010100970703e   1C                   ENDIF
010200971020     C*
010300971020     C* operazioni finali
010400971020     C                   EXSR      finsr
010500971020     C*
010600970610     C                   SETON                                            lr
010700970610     c*--------------------------------------------------------------------------------------------*
010800970610     c* setfil - posizionamento sul file e prima lettura
010900970610     c*--------------------------------------------------------------------------------------------*
011000970610     C     setfil        BEGSR
011100970610     C*
011200970610     C* posizionamento sul file
011300970610     c     *loval        SETLL     azorg01l                           99
011400970610if  1c                   IF        NOT *in99
011500970610     c                   EXSR      letfil                                       *lettura record
011600970610x   1c                   ELSE
011700970610     C                   MOVEL     'S'           $fine                          *fine programma
011800970610e   1C                   ENDiF
011900970610     C*
012000970610     C                   ENDSR
012100970610     c*--------------------------------------------------------------------------------------------*
012200970610     c* letfil - lettura record
012300970610     c*--------------------------------------------------------------------------------------------*
012400970610     C     letfil        BEGSR
012500970610     C*
012600970610     C* Legge fino a:
012700970610     C*       -fine file
012800970610     C*       -trovato record valido
012900970610     c                   MOVEL     'N'           $recok                         *record non valido
013000970610do  1c                   DOW       NOT *in99 AND
013100970610     c                             $recok='N'
013200970611     C                   READ      azorg01l                               99
013300970610if  2c                   IF        *in99                                        *fine file
013400970610     c                   MOVEL     'S'           $fine                          *fine programma
013500970610x   2c                   ELSE                                                   *record trovato
013600970610     c                   EXSR      chkrec                                       *controlla record
013700970610e   2c                   ENDIF
013800970610e   1c                   ENDDO
013900970610     C*
014000970610     C                   ENDSR
014100970610     c*--------------------------------------------------------------------------------------------*
014200970610     c* chkrec - controlla validità record
014300970610     c*--------------------------------------------------------------------------------------------*
014400970610     C     chkrec        BEGSR
014500970610     C*
014600970610     c                   MOVEL     'S'           $recok                         *record valido
014700970610     C*
014800970610     C* filiale annullata, esclude
014900970610if  1c                   IF        orgfva<>*blanks
015000970610     c                   MOVEL     'N'           $recok                         *record non valido
015100970610e   1c                   ENDIF
015200970610     C*
015300970610     C* filiale varia, esclude
015400970610if  1c                   IF        orgfag<>'F' AND
015500970610     c                             orgfag<>'A'
015600970610     c                   MOVEL     'N'           $recok                         *record non valido
015700970610e   1c                   ENDIF
015800970610     C*
015900970610     C                   ENDSR
016000971020     c*--------------------------------------------------------------------------------------------*
016100971020     C* rectfx - recupera i terminal della filiale
016200971020     c*--------------------------------------------------------------------------------------------*
016300971020     C     rectfx        BEGSR
016400971020     C*
016500971020     C                   CLEAR                   dslv55
016600971020     C                   MOVEL     *blanks       d55tla                         *tipo lancio
016700971020     C                   MOVEL     'A'           d55tpt                         *eccezione arrivo
016800971020     C                   Z-ADD     orgfil        d55lin                         *linea
016900971020     C                   MOVEL     p50fil        d55lnp                         *terminal partenza
017000971020     C                   MOVEL     p50dri        d55drf                         *data riferimento
017100971020     C                   CALL      'FNLV55R'
017200971020     C                   PARM                    dslv55
017300971020     C*
017400971020     C                   ENDSR
017500970610     c*--------------------------------------------------------------------------------------------*
017600970610     C* scrfil0 - scrive la filiale sul file di output
017700970610     c*--------------------------------------------------------------------------------------------*
017800970610     C     scrfil0       BEGSR
017900970610     C*
018000970610     C* imposta i campi del record da scrivere
018100970611     C                   Z-ADD     orgfil        a_filfil
018200970611if  1c                   IF        orgde5<>*blanks
018300970610     C                   MOVEL(P)  orgde5        a_fildes
018400970611x   1c                   ELSE
018500970611     C                   MOVEL(P)  orgdes        a_fildes
018600970611e   1c                   ENDIF
018700970610     C                   MOVEL     orgind        a_filind
018800970610     C                   Z-ADD     orgcpf        a_filcpf
018900970610     C                   MOVEL     orgloc        a_filloc
019000970610     C                   MOVEL     orgpro        a_filpro
019100970610     C                   MOVEL     orgtel        a_filtel
019200970610     C                   MOVEL     orgfax        a_filfax
019300970610     C                   MOVEL     orgtlx        a_filtlx
019400010921     C                   IF        p50azz = 'S'
019500010921     C                   EVAL      a_filtrm = 789
019600010921     C                   ELSE
019700010921     C                   Z-ADD     d55tfa        a_filtrm
019800010921     C                   ENDIF
019900970610     C*
020000970610     C* scrive il record
020100970610     C                   WRITE     azfil0
020200970610     C*
020300970610     C                   ENDSR
020400970610     c*--------------------------------------------------------------------------------------------*
020500970610     C* scrfil1 - scrive la filiale sul file di output
020600970610     c*--------------------------------------------------------------------------------------------*
020700970610     C     scrfil1       BEGSR
020800970610     C*
020900970610     C* imposta i campi del record da scrivere
021000970610     C                   MOVEl     orgfil        b_filfil
021100970611if  1c                   IF        orgde5<>*blanks
021200970611     C                   MOVEL(P)  orgde5        b_fildes
021300970611x   1c                   ELSE
021400970611     C                   MOVEL(P)  orgdes        b_fildes
021500970611e   1c                   ENDIF
021600970610     C                   MOVEL     orgind        b_filind
021700970610     C                   MOVEL     orgcpf        b_filcpf
021800970610     C                   MOVEL     orgloc        b_filloc
021900970610     C                   MOVEL     orgpro        b_filpro
022000970610     C                   MOVEL     orgtel        b_filtel
022100970610     C                   MOVEL     orgfax        b_filfax
022200970610     C                   MOVEL     orgtlx        b_filtlx
022300010921     C                   IF        p50azz = 'S'
022400010921     C                   EVAL      b_filtrm = '789'
022500010921     C                   ELSE
022600971020     C                   MOVEL     d55tfa        b_filtrm
022700010921     C                   ENDIF
022800970610     C*
022900970610     C* scrive il record
023000970610     C                   WRITE     azfil1
023100970610     C*
023200970610     C                   ENDSR
023300970618     c*--------------------------------------------------------------------------------------------*
023400970618     C* imppar - Imposta le variabili di lavoro in base ai parametri di entrata
023500970618     c*--------------------------------------------------------------------------------------------*
023600970618     C     imppar        BEGSR
023700970703     C*
023800970703     C* reperimento versione cappario
023900970703if  1c                   IF        p50dri=*blanks
024000970707     c                   Z-ADD     *zeros        p96dri
024100970703x   1c                   ELSE
024200970707     c                   MOVEL     p50dri        p96dri
024300970703e   1c                   ENDIF
024400970707     c                   Z-ADD     *zeros        p96ver
024500970707     c                   CALL      'TISI96R'
024600970707     c                   PARM                    p96dri
024700970707     c                   PARM                    p96ver
024800970707if  1c                   IF        p96dri=*zeros                                *not version found
024900970703     c                   MOVEL     '1'           $err                           *errore
025000970703e   1c                   ENDIF
025100970703     C*
025200970703     C* imposta i campi se non passati
025300970703if  1c                   IF        p50fil=*blanks                               parametri no passati
025400970703     c                   MOVEL     simfel        p50fil                         *filiale
025500970703e   1c                   ENDIF
025600970703if  1c                   IF        p50dri=*blanks                               parametri no passati
025700971020     c                   MOVEL     datcor        p50dri                         *data riferimento
025800970703e   1c                   ENDIF
025900970618     C*
026000970618     C                   ENDSR
026100971020     c*--------------------------------------------------------------------------------------------*
026200971020     C* finsr - operazioni finali
026300971020     c*--------------------------------------------------------------------------------------------*
026400971020     C     finsr         BEGSR
026500971020     C*
026600971020     C                   CLEAR                   dslv55
026700971020     C                   MOVEL     'C'           d55tla
026800971020     C                   CALL      'FNLV55R'
026900971020     C                   PARM                    dslv55
027000971020     C*
027100971020     C                   ENDSR
027200970610     c*--------------------------------------------------------------------------------------------*
027300970610     C* *inzsr - operazioni iniziali
027400970610     c*--------------------------------------------------------------------------------------------*
027500970610     C     *inzsr        BEGSR
027600970610     c*
027700970610     c* ricevimento parametri
027800970610     c     *ENTRY        PLIST
027900970610     c                   PARM                    kpjba
028000970610     c                   MOVEL     kpjbu         dspar50
028100970610     C*
028200970610     C* reperimento data corrente
028300970610     C                   TIME                    wn14                           *ora (6)+ data (8)
028400970610     C                   MOVE      wn14          wn8                            *data (8) in g/m/aa
028500971020     C                   Z-ADD     wn8           g08dat
028600971020     C                   Z-ADD     *zeros        g08inv
028700971020     C                   MOVEL     '0'           g08err
028800971020     c                   CALL      'XSRDA8'
028900971020     c                   PARM                    wlbda8
029000971020     C                   Z-ADD     g08inv        datcor                         *Data corrente
029100970618     C*
029200970618     C* reperimento dati utente
029300020906     c                   Z-ADD     1             codut
029400970618     C                   CALL      'X§PARUT'
029500970618     C                   PARM                    ut§dse
029600970618     C                   MOVEL     rec80         cncr80
029700970610     C*
029800970610     C                   ENDSR
