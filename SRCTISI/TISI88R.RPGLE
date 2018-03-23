000100970603     H*--------------------------------------------------------------------------------------------*
000200970603     H* Aggiorna una località dai suoi archivi
000300970603     H*--------------------------------------------------------------------------------------------*
000400970603     H DECEDIT('0,') DATEDIT(*DMY.)
000500970603     F*--------------------------------------------------------------------------------------------*
000600970603     F* Data base
000700970603     F*--------------------------------------------------------------------------------------------*
000800970609     d*--------------------------------------------------------------------------------------------*
000900970609     d* Data structure
001000970609     d*--------------------------------------------------------------------------------------------*
001100970619     D par             S             35    DIM(20) INZ                          *Parole
001200970623     D parl            S             35    DIM(20) INZ                          *Parole località
001300970623     D parsan          S              1    DIM(20) INZ                          *Parole 'SANTE'
001400970623     D parcol          S              1    DIM(20) INZ                          *Parole collegamento
001500970620     D parspa          S              1  0 DIM(20) INZ(1)                       *Spazi dopo
001600970623     D sin             S             35    DIM(60)                              *Sinonimi
001700970603     D*----------------
001800970603     D* Variabili riferite al data base
001900970603     D*----------------
002000970603     D*----------------
002100970603     D* Variabili a programma
002200970603     D*----------------
002300970619     D wloc            S             35                                         *localita lavoro
002400970619     D pos             S              3  0                                      *posiz.del blank
002500970619     D p               S              3  0                                      *indice di PAR
002600970620     D i               S              3  0                                      *indice
002700970623     D x               S              3  0                                      *indice
002800970620     D pp              S              3  0                                      *indice di PAR
002900970620     D nsin            S              3  0                                      *numero sin.creati
003000970620     D wsin            S             35                                         *sinonimo work
003100970619     D api             S              1    INZ('''')                            *apice
003200970623     D fapi            S              1                                         *flag apice
003300970620     D npar            S              3  0                                      *n.parole in schiera
003400970620     D spa             S              1  0                                      *n.spazi dopo parola
003500970609     d*----------------
003600970609     d* Ds
003700970609     d*----------------
003800970619     d dssi88        E DS                  EXTNAME(tisi88ds)                    *ds di procedura
003900970609     D kpjba         E DS                                                       *Architettura
004000970603     C*--------------------------------------------------------------------------------------------*
004100970603     C* Main lines
004200970603     C*--------------------------------------------------------------------------------------------*
004300970603     C*
004400970623     C* scompone la localita in parole lasciando apici e parole di collegamento (in,sul,al,..)
004500970623     c                   MOVEL     'S'           fapi
004600970619     C                   EXSR      parole
004700970620     C*
004800970623     C* crea i sinonimi della località con parole di collegamento
004900970620     C                   EXSR      creasin
005000970619     C*
005100970623     C* scompone la localita in parole togliendo apici e parole di collegamento
005200970623     c                   MOVEL     'N'           fapi
005300970623     C                   EXSR      parole
005400970623     C*
005500970623     C* elimina le parole di collegamento
005600970619     C                   EXSR      elimina
005700970603     C*
005800970623     C* crea i sinonimi della località senza parole di collegamento
005900970620     C                   EXSR      creasin
006000970620     C*
006100970620     C* porta sinonimi in output
006200970620     C                   EXSR      sinout
006300970603     C*
006400970603     C                   SETON                                        lr
006500970603     C*--------------------------------------------------------------------------------------------*
006600970619     C* parole - Recupera tutte le parole della localita
006700970603     C*--------------------------------------------------------------------------------------------*
006800970619     C     parole        BEGSR
006900970603     c*
007000970623     c                   MOVEA     *blanks       par
007100970623     c*
007200970623     c* toglie spazi destra/sinistra alla località
007300970619     c                   EVAL      wloc = %TRIM ( i88loc )
007400970623     C*
007500970619     c* sostituisce "'" con spazio
007600970623     c     fapi          IFNE      'S'
007700970623     c     api:' '       XLATE     wloc          wloc
007800970623     c                   ENDIF
007900970619     c*
008000970619     c                   Z-ADD     *zeros        p
008100970619     c*
008200970619     c* ciclo per recuperare le parole
008300970619     c     ' '           SCAN      wloc          pos                      99
008400970619     c                   DOW       pos > 1 AND *in99='1'
008500970619     c                   ADD       1             p
008600970619     c                   EVAL      par(p) = %SUBST(wloc:1:pos-1)
008700970619     c                   EVAL      wloc = %TRIM(%SUBST(wloc:pos+1))
008800970619     c     ' '           SCAN      wloc          pos                      99
008900970619     c                   ENDDO
009000970623     c*
009100970623     c* salva parole localita' in altra schiera
009200970623     c                   MOVEA     par           parl
009300970603     c*
009400970603     C                   ENDSR
009500970619     C*--------------------------------------------------------------------------------------------*
009600970623     C* attrib - Determina attributi delle parole
009700970619     C*--------------------------------------------------------------------------------------------*
009800970623     C     attrib        BEGSR
009900970619     c*
010000970623     c                   CLEAR                   parcol
010100970623     c                   CLEAR                   parsan
010200970623     c*
010300970619     c* ciclo per tutte le parole
010400970623     c                   DO        20            x
010500970623     c* collegamento
010600970623     c     par(x)        IFEQ      'A'
010700970623     c     par(x)        OREQ      'AL'
010800970623     c     par(x)        OREQ      'ALL'
010900970623     c     par(x)        OREQ      'ALLA'
011000970623     c     par(x)        OREQ      'ALLE'
011100970623     c     par(x)        OREQ      'CON'
011200970623     c     par(x)        OREQ      'D'
011300970703     c     par(x)        OREQ      'DA'
011400970623     c     par(x)        OREQ      'DI'
011500970623     c     par(x)        OREQ      'DE'
011600970623     c     par(x)        OREQ      'DEL'
011700970623     c     par(x)        OREQ      'DELL'
011800970623     c     par(x)        OREQ      'DELLA'
011900970623     c     par(x)        OREQ      'DELLE'
012000970623     c     par(x)        OREQ      'DELLO'
012100970623     c     par(x)        OREQ      'DEI'
012200970623     c     par(x)        OREQ      'DEGLI'
012300970623     c     par(x)        OREQ      'E'
012400970623     c     par(x)        OREQ      'IN'
012500970627     c     par(x)        OREQ      'IL'
012600970627     c     par(x)        OREQ      'L'
012700970627     c     par(x)        OREQ      'LA'
012800970627     c     par(x)        OREQ      'LE'
012900970623     c     par(x)        OREQ      'NEL'
013000970623     c     par(x)        OREQ      'NELL'
013100970623     c     par(x)        OREQ      'NEI'
013200970623     c     par(x)        OREQ      'SU'
013300970623     c     par(x)        OREQ      'SUL'
013400970623     c     par(x)        OREQ      'SULL'
013500970623     c     par(x)        OREQ      'SULLE'
013600970623     c     par(x)        OREQ      'SULLA'
013700970623     c     par(x)        OREQ      'SUI'
013800970623     c                   MOVEL     'S'           parcol(x)
013900970620     c                   ENDIF
014000970623     c* santi
014100970623     c     par(x)        IFEQ      'S'
014200970623     c     par(x)        OREQ      'S.'
014300970623     c     par(x)        OREQ      'ST.'
014400970623     c     par(x)        OREQ      'SAN'
014500970623     c     par(x)        OREQ      'SANT'
014600970623     c     par(x)        OREQ      'SANTA'
014700970623     c     par(x)        OREQ      'SANTO'
014800970623     c     par(x)        OREQ      'SANTI'
014900970623     c     par(x)        OREQ      'SAINT'
015000970623     c                   MOVEL     'S'           parsan(x)
015100970623     c                   ENDIF
015200970623     c*
015300970619     c                   ENDDO
015400970619     c*
015500970619     C                   ENDSR
015600970620     C*--------------------------------------------------------------------------------------------*
015700970620     C* elimina - Elimina tutte le parole di collegamento
015800970620     C*--------------------------------------------------------------------------------------------*
015900970620     C     elimina       BEGSR
016000970620     c*
016100970623     c                   MOVEA     parl          par
016200970623     c                   EXSR      attrib
016300970623     c*
016400970620     c* ciclo per tutte le parole
016500970620     c                   DO        20            p
016600970623     c     parcol(p)     IFEQ      'S'
016700970620     c                   EVAL      par(p) = *blanks
016800970620     c                   ENDIF
016900970620     c                   ENDDO
017000970623     c                   EXSR      compatta                                     *compatta sch.par
017100970623     c                   MOVEA     par           parl
017200970620     c*
017300970620     C                   ENDSR
017400970620     C*--------------------------------------------------------------------------------------------*
017500970620     C* compatta - Compatta schiera parole togliendo indici vuoti
017600970620     C*--------------------------------------------------------------------------------------------*
017700970620     C     compatta      BEGSR
017800970620     c*
017900970620     c     1             DO        19            pp
018000970620     c                   IF        par(pp+1) <> *blanks AND par(pp) = *blanks
018100970620     c                   EVAL      par(pp)=par(pp+1)
018200970620     c                   EVAL      par(pp+1) = *blanks
018300970620     c                   ENDIF
018400970620     C                   ENDDO
018500970620     c*
018600970620     C                   ENDSR
018700970620     C*--------------------------------------------------------------------------------------------*
018800970620     C* creasin - Crea i sinonimi della località
018900970620     C*--------------------------------------------------------------------------------------------*
019000970620     C     creasin       BEGSR
019100970620     c*
019200970620     c* ciclo per i SANTI
019300970623     c                   MOVEA     parl          par
019400970620     c                   DO        20            p
019500970623     c                   MOVEA     1             parspa
019600970620SL  4c                   SELECT
019700970620     c     par(p)        WHENEQ    'ST.'                                        ST.
019800970620     c     par(p)        OREQ      'SANT'                                       SANT
019900970620     c     par(p)        OREQ      'SANTA'                                      SANTA
020000970620     c     par(p)        OREQ      'SAINT'                                      SAINT
020100970620     c     par(p)        OREQ      'SANTO'                                      SANTO
020200970620     c                   Z-ADD     1             parspa(p)
020300970620     c                   EXSR      crnewsin
020400970620     c                   MOVEL     *blanks       par(p)
020500970620     c                   MOVEL     'S'           par(p)
020600970620     c                   Z-ADD     1             parspa(p)
020700970620     c                   EXSR      crnewsin
020800970620     c                   MOVEL     *blanks       par(p)
020900970620     c                   MOVEL     'S.'          par(p)
021000970620     c                   Z-ADD     1             parspa(p)
021100970620     c                   EXSR      crnewsin
021200970620     c                   MOVEL     *blanks       par(p)
021300970620     c                   MOVEL     'S.'          par(p)
021400970620     c                   Z-ADD     0             parspa(p)
021500970620     c                   EXSR      crnewsin
021600970620     c                   MOVEL     *blanks       par(p)
021700970620     c                   MOVEL     'SAN'         par(p)
021800970620     c                   Z-ADD     1             parspa(p)
021900970620     c                   EXSR      crnewsin
022000970623     c     p             IFGT      1
022100970620     c                   MOVEL     *blanks       par(p)
022200970620     c                   Z-ADD     1             parspa(p)
022300970620     c                   EXSR      crnewsin
022400970623     c                   ENDIF
022500970620     c     par(p)        WHENEQ    'SAN'                                        SAN
022600970620     c                   Z-ADD     1             parspa(p)
022700970620     c                   EXSR      crnewsin
022800970620     c                   MOVEL     *blanks       par(p)
022900970620     c                   MOVEL     'S'           par(p)
023000970620     c                   Z-ADD     1             parspa(p)
023100970620     c                   EXSR      crnewsin
023200970620     c                   MOVEL     *blanks       par(p)
023300970620     c                   MOVEL     'S.'          par(p)
023400970620     c                   Z-ADD     1             parspa(p)
023500970620     c                   EXSR      crnewsin
023600970620     c                   MOVEL     *blanks       par(p)
023700970620     c                   MOVEL     'S.'          par(p)
023800970620     c                   Z-ADD     0             parspa(p)
023900970620     c                   EXSR      crnewsin
024000970620     c     p             IFGT      1
024100970620     c                   MOVEL     *blanks       par(p)
024200970620     c                   Z-ADD     1             parspa(p)
024300970620     c                   EXSR      crnewsin
024400970620     c                   ENDIF
024500970620     c     par(p)        WHENEQ    'SANTI'                                      SANTI
024600970620     c                   Z-ADD     1             parspa(p)
024700970620     c                   EXSR      crnewsin
024800970620     c                   MOVEL     *blanks       par(p)
024900970620     c                   MOVEL     'SS'          par(p)
025000970620     c                   Z-ADD     1             parspa(p)
025100970620     c                   EXSR      crnewsin
025200970620     c                   MOVEL     *blanks       par(p)
025300970620     c                   MOVEL     'S.S.'        par(p)
025400970620     c                   Z-ADD     1             parspa(p)
025500970620     c                   EXSR      crnewsin
025600970620     c                   MOVEL     *blanks       par(p)
025700970620     c                   MOVEL     'S.S.'        par(p)
025800970620     c                   Z-ADD     0             parspa(p)
025900970620     c                   EXSR      crnewsin
026000970620     c                   MOVEL     *blanks       par(p)
026100970620     c                   MOVEL     'SAN'         par(p)
026200970620     c                   Z-ADD     1             parspa(p)
026300970620     c                   EXSR      crnewsin
026400970620     c     p             IFGT      1
026500970620     c                   MOVEL     *blanks       par(p)
026600970620     c                   Z-ADD     1             parspa(p)
026700970620     c                   EXSR      crnewsin
026800970620     c                   ENDIF
026900970623     c                   OTHER
027000970623     c                   EXSR      crnewsin
027100970620     c                   ENDSL
027200970620     c                   ENDDO
027300970620     c*
027400970620     c* genera i nuovi sinonimi togliendo ultime parole
027500970623     c*---               EXSR      crnewsin
027600970620     c*
027700970620     C                   ENDSR
027800970620     C*--------------------------------------------------------------------------------------------*
027900970620     C* crnewsin - Crea un nuovi sinonimi dalla schiera parole
028000970620     C*--------------------------------------------------------------------------------------------*
028100970620     C     crnewsin      BEGSR
028200970620     c*
028300970620     c* compatta schiera parole
028400970620     c                   EXSR      compatta
028500970623     c*
028600970623     c* ricerca attributi parole (santa,collegam.)
028700970623     c                   EXSR      attrib
028800970623     c*
028900970623     c* conta il n° di parole in schiera
029000970620     c                   EVAL      npar = %ELEM(par)
029100970623     c*
029200970623     c* ciclo per il n° di parole
029300970620     c                   DOU       npar < 1
029400970623     c*
029500970623     c* se l'ultima parola è SANTA o di collegamento non la scrive
029600970623     c     parsan(npar)  IFEQ      'S'
029700970623     c     parcol(npar)  OREQ      'S'
029800970620     c                   MOVEL     *blanks       par(npar)
029900970620     c                   ENDIF
030000970620     c                   MOVEL     *blanks       wsin
030100970620     c                   DO        npar          pp
030200970620     c                   EVAL      wsin = %TRIM(wsin)
030300970623     c     pp            IFGT      1
030400970623     c                   EVAL      spa=parspa(pp-1)
030500970623     c                   ENDIF
030600970620     c     wsin          CAT       par(pp):spa   wsin
030700970620     c                   ENDDO
030800970620     c                   EVAL      wsin = %TRIM(wsin)
030900970623     c*
031000970620     c* porta sinonimo nella schiera output
031100970620     c     wsin          IFNE      *blanks
031200970620     c     wsin          ANDNE     i88loc                                       *diverso loc.input
031300970620     c     wsin          LOOKUP    sin                                    99    *cerco nei sin out
031400970620     c     *in99         IFEQ      *off                                         *non trovato
031500970620     c                   ADD       1             nsin
031600970620     c                   MOVEL     wsin          sin(nsin)
031700970620     c                   ENDIF
031800970620     c                   ENDIF
031900970623     c*
032000970620     c* toglie una parola e genera altro sinonimo
032100970620     c                   SUB       1             npar
032200970620     c                   ENDDO
032300970623     c                   MOVEA     parl          par
032400970620     c*
032500970620     C                   ENDSR
032600970603     C*--------------------------------------------------------------------------------------------*
032700970619     C* sinout - Porta sinonimi trovati in output
032800970603     C*--------------------------------------------------------------------------------------------*
032900970619     C     sinout        BEGSR
033000970603     c*
033100970620     c                   MOVEA     sin           o88sin
033200970603     c*
033300970603     C                   ENDSR
033400970603     C*--------------------------------------------------------------------------------------------*
033500970603     C* *inzsr - Operazioni iniziali
033600970603     C*--------------------------------------------------------------------------------------------*
033700970603     C     *inzsr        BEGSR
033800970603     C*---
033900970603     C* ricevimento parametri
034000970603     C*---
034100970603     C     *ENTRY        PLIST
034200970619     C                   PARM                    dssi88
034300970603     C*---
034400970603     C* chiavi di lettura
034500970603     C*---
034600970603     C*
034700970603     C                   ENDSR
