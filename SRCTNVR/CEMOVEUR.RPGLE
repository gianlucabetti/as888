000100011123      *--------------------------------------------------------------------------------------------*
000200011123      * Utility - Da WFMOV scrive il nuovo ECMOV
000300011123      *--------------------------------------------------------------------------------------------*
000400011123     H DECEDIT('0,') DATEDIT(*DMY.)
000500011123      *--------------------------------------------------------------------------------------------*
000600011123      * Data base
000700011123      *--------------------------------------------------------------------------------------------*
000800011123      *---
000900011123      * Movimenti C/E vecchi
001000011123      *---
001100011123     Fwfmov01l  IF   E           K DISK
001200011123      *---
001300011123      * Movimenti C/E nuovi
001400011123      *---
001500011126     Fecmov01L  O    E           K DISK
001600011123      *--------------------------------------------------------------------------------------------*
001700011123      * Data structure
001800011123      *--------------------------------------------------------------------------------------------*
001900011123      *---
002000011123      * Prefisso variabili --> § = passaggio routine esterne
002100011123      *                    --> w = passaggio routine interne
002200011123      *                    --> $ = di controllo
002300011123      *---
002400011123      *---
002500011123      * Variabili riferite al data base
002600011123      *---
002700011123     d kfmcdp          S                   LIKE(wfmcdp)                         *wfmov01l
002800011123     d kfmkcc          S                   LIKE(wfmkcc)
002900011123     d kfmksc          S                   LIKE(wfmksc)
003000011123     d kfmcdc          S                   LIKE(wfmcdc)
003100011123     d kfmda           S                   LIKE(wfmda)
003200011123     d kovsoc          S                   LIKE(movsoc)                         *ecmov01l
003300011123     d kovcdp          S                   LIKE(movcdp)
003400011123     d kovsvo          S                   LIKE(movsvo)
003500011123     d kovcdc          S                   LIKE(movcdc)
003600011123     d kovda           S                   LIKE(movda)
003700011123      *---
003800011123      * variabili
003900011123      *---
004000011126     d n14             S             14  0
004100011126     d n8              S              8  0
004200011126     d a7              S              7
004300011126     d datcor          S              8  0                                      *data corrente
004400011123     d $fine           S              1    INZ('N')                             *fine programma
004500011123     d $recok          S              1    INZ('N')                             *validità record
004600011126      *---
004700011126      * conversione data giuliano/gregoriano
004800011126      *---
004900011126     d §datgre         S              6                                         *data gregoriano
005000011126     d §datgiu         S              5                                         *data giuliano
005100011126     d §fmtdat         S              1                                         *fromato data
005200011126     d*----------------
005300011126     d* Ds
005400011126     d*----------------
005500011126      *---
005600011126      * data (6)
005700011126      *---
005800011126     d                 DS
005900011126     d  dann6                  1      2  0                                       -anno
006000011126     d  dmes6                  3      4  0                                       -mese
006100011126     d  dgio6                  5      6  0                                       -giorno
006200011126     d  dsdat6                 1      6  0                                      *data (6)
006300011126      *---
006400011126      * data (8)
006500011126      *---
006600011126     d                 DS
006700011126     d  dsec8                  1      2  0                                       -secolo
006800011126     d  dann8                  3      4  0                                       -anno
006900011126     d  dmes8                  5      6  0                                       -mese
007000011126     d  dgio8                  7      8  0                                       -giorno
007100011126     d  dsdat8                 1      8  0                                      *data (8)
007200011126      *---
007300011126      * controlla data
007400011126      *---
007500011126     d §wlbda8         DS                  INZ                                  *controlla data
007600011126     d  §g08dat                       8  0
007700011126     d  §g08inv                       8  0
007800011126     d  §g08err                       1
007900011126     d  §g08tgi                       5  0
008000011126      *---
008100011126      * converte importi
008200011126      *---
008300011126     d yeurcods      E DS
008400011123      *--------------------------------------------------------------------------------------------*
008500011123      * Main lines
008600011123      *--------------------------------------------------------------------------------------------*
008700011123      *
008800011123      * posizionamento sul file e prima lettura record
008900011123     c                   EXSR      setfil
009000011123      *
009100011123      * ciclo fino a fine file
009200011123do  1c                   DOW       $fine='N'
009300011123      *
009400011123      * elabora record
009500011123     c                   EXSR      elarec
009600011123      *
009700011123      * lettura successiva record
009800011123     C                   EXSR      letfil
009900011123e   1c                   ENDDO
010000011123      *
010100011123     C                   SETON                                            lr
010200011123      *--------------------------------------------------------------------------------------------*
010300011123      * posizionamento sul file e prima lettura record
010400011123      *--------------------------------------------------------------------------------------------*
010500011123     C     setfil        BEGSR
010600011123      *
010700011123     c     *loval        SETLL     wfmov01l                           99
010800011123if  1c                   IF        NOT *in99
010900011123     c                   EXSR      letfil                                       *lettura record
011000011123x   1c                   ELSE
011100011123     C                   MOVEL     'S'           $fine                          *fine file
011200011123e   1C                   ENDIF
011300011123      *
011400011123     C                   ENDSR
011500011123      *--------------------------------------------------------------------------------------------*
011600011123      * lettura successiva record
011700011123      *--------------------------------------------------------------------------------------------*
011800011123     C     letfil        BEGSR
011900011123      *
012000011123      * Legge fino a:
012100011123      *       -fine file
012200011123      *       -trovato record valido
012300011123     c                   MOVEL     'N'           $recok                         *record non valido
012400011123do  1c                   DOW       NOT *in99 AND
012500011123     c                             $recok='N'
012600011123     C                   READ      wfmov01l                               99
012700011123if  2c                   IF        *in99
012800011123     C                   MOVEL     'S'           $fine                          *fine file
012900011123x   2c                   ELSE
013000011123     c                   EXSR      chkrec                                       *controlla record
013100011123e   2c                   ENDIF
013200011123e   1c                   ENDDO
013300011123      *
013400011123     C                   ENDSR
013500011123      *--------------------------------------------------------------------------------------------*
013600011123      * controlla validità record
013700011123      *--------------------------------------------------------------------------------------------*
013800011123     C     chkrec        BEGSR
013900011123      *
014000011123     c                   MOVEL     'S'           $recok                         *record valido
014100011123      *
014200011123     c                   ENDSR
014300011123      *--------------------------------------------------------------------------------------------*
014400011123      * elabora record
014500011123      *--------------------------------------------------------------------------------------------*
014600011123     C     elarec        BEGSR
014700011123      *
014800011123     c                   MOVEL     'S'           $recok                         *record valido
014900011123      *
015000011126      * imposta campi
015100011126     c                   EXSR      impmov
015200011126      *
015300011126      * scrive il record
015400011126     c                   WRITE     ecmov000
015500011126      *
015600011123     c                   ENDSR
015700011126      *--------------------------------------------------------------------------------------------*
015800011126      * imposta campi
015900011126      *--------------------------------------------------------------------------------------------*
016000011126     C     impmov        BEGSR
016100011126      *
016200011126     c                   EVAL      movsoc = '201'                               *società
016300011126      *
016400011126     c                   EVAL      movtmo = 'G'                                 *tipo registrazione
016500011126      *
016600011126     c                   EVAL      movcot = 'COMP'                              *causale testata
016700011126      *
016800011126     c                   EVAL      movctb = 'CE'                                *contabilità
016900011126      *
017000011126     c                   Z-ADD     wfmdrg        dsdat6
017100011126     c                   Z-ADD     dsdat6        dsdat8
017200011126     c                   Z-ADD     20            dsec8
017300011126     c                   Z-ADD     dsdat8        movdrg                         *data registrazione
017400011126      *
017500011126     c                   Z-ADD     wfmnrg        movnrg                         *n° registrazione
017600011126      *
017700011126     c                   Z-ADD     wfmcdp        dsdat6
017800011126     c                   Z-ADD     dsdat6        dsdat8
017900011126     c                   Z-ADD     20            dsec8
018000011126     c                   Z-ADD     dsdat8        movcdp                         *data competenza
018100011126      *
018200011126     c                   EVAL      movkcc = *blanks                             *capoconto
018300011126      *
018400011126     c                   EVAL      movksc = *blanks                             *sottoconto
018500011126      *
018600011126     c                   Z-ADD     wfmimp        movimp                         *importo
018700011126if  1c                   IF        wfmimp <> *zeros
018800011126     C                   CLEAR                   YEURCODS
018900011126     C                   MOVEL     'ITL'         YECDVI
019000011126     C                   Z-ADD     wfmimp        YECIMI
019100011126     C                   MOVEL     'EUR'         YECDVO
019200011126     C                   MOVEL     3             YECDCO
019300011126     C                   CALL      'YEURCO'
019400011126     C                   PARM                    YEURCODS
019500011126if  2C                   IF        YECESI = *blanks
019600011126     C                   Z-ADD     YECIMO        movimp
019700011126e   2C                   ENDIF
019800011126e   1C                   ENDIF
019900011126      *
020000011126     c                   EVAL      movdiv = 'EUR'                               *divisa
020100011126      *
020200011126     c                   EVAL      movda  = wfmda                               *dare / avere
020300011126      *
020400011126     c                   EVAL      movdes = wfmdes                              *descrizione
020500011126      *
020600011126     c                   EVAL      movvoc = wfmfla                              *voce
020700011126      *
020800011126     c                   MOVEL     wfmcdc        a7
020900011126     c                   EVAL      movcdc = a7                                  *centro di costo
021000011126      *
021100011126     c                   EVAL      moven2 = *blanks                             *entità 2
021200011126      *
021300011126     c                   EVAL      moven3 = *blanks                             *entità 3
021400011126      *
021500011126     c                   Z-ADD     wfmksc        movsvo                         *sotto voce
021600011126      *
021700011126     c                   MOVEL     *blanks       §datgre
021800011126     c                   MOVEL     wfmnu1        §datgiu
021900011126     c                   MOVEL     '2'           §fmtdat
022000011126     c                   CALL      'XCVTDAT'
022100011126     c                   PARM                    §datgre
022200011126     c                   PARM                    §datgiu
022300011126     c                   PARM                    §fmtdat
022400011126     c                   MOVEL     §datgre       dsdat6
022500011126     c                   Z-ADD     dsdat6        dsdat8
022600011126     c                   Z-ADD     20            dsec8
022700011126     c                   Z-ADD     dsdat8        movduv                         *dta umtima modifica
022800011126      *
022900011126     c                   ENDSR
023000011123      *--------------------------------------------------------------------------------------------*
023100011123      * Operazioni iniziali -una volta sola-
023200011123      *--------------------------------------------------------------------------------------------*
023300011123     C     *inzsr        BEGSR
023400011123      *
023500011123      * chiavi di lettura
023600011123     C     keywfm        KLIST                                                  *wfmov01l
023700011123     C                   KFLD                    kfmcdp                          -data competenza
023800011123     C                   KFLD                    kfmkcc                          -capoconto
023900011123     C                   KFLD                    kfmksc                          -sottoconto
024000011123     C                   KFLD                    kfmcdc                          -centro di costo
024100011123     C                   KFLD                    kfmda                           -dare/avere
024200011123     C     keymov        KLIST                                                  *ecmov01l
024300011123     C                   KFLD                    kovsoc                          -società
024400011123     C                   KFLD                    kovcdp                          -data competenza
024500011123     C                   KFLD                    kovsvo                          -sotto voce
024600011123     C                   KFLD                    kovcdc                          -centro di costo
024700011123     C                   KFLD                    kovda                           -dare/avere
024800011126      *
024900011126      * reperisce data corrente
025000011126     C                   TIME                    n14                            *ora (6)+ data (8)
025100011126     C                   MOVE      n14           n8                             *data (8) in g/m/aa
025200011126     C                   Z-ADD     n8            §g08dat
025300011126     C                   Z-ADD     *zeros        §g08inv
025400011126     C                   MOVEL     '0'           §g08err
025500011126     C                   CALL      'XSRDA8'
025600011126     C                   PARM                    §wlbda8
025700011126     C                   Z-ADD     §g08inv       datcor                         *data corrente a/m/g
025800011123      *
025900011123     C                   ENDSR
