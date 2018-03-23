000100970605     h*--------------------------------------------------------------------------------------------*
000200980915     h* Ricezione Documenti richiesti relativi ad una CA
000300970605     h*--------------------------------------------------------------------------------------------*
000400970605     h DECEDIT('0,') DATEDIT(*DMY.)
000500970605     f*--------------------------------------------------------------------------------------------*
000600970605     f* Data base
000700970605     f*--------------------------------------------------------------------------------------------*
000800970605     fcmric01l  UF   E           K DISK
000900981013     ftntbe01l  UF A E           K DISK
001000970611     f                                     PREFIX(f_)
001100981013     ftntbe00r  UF   E             DISK    USROPN
001200981013     f                                     RENAME(tntbe000:tntber)
001300970611     f                                     PREFIX(r_)
001400970605     d*--------------------------------------------------------------------------------------------*
001500970605     d* Data structure
001600970605     d*--------------------------------------------------------------------------------------------*
001700970605     d*----------------
001800970605     d* Schiere
001900970605     d*----------------
002000980915     d cm1             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002100980915     d cm2             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002200980915     d cm3             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002300980915     d cm4             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002400980915     d cm5             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002500970605     d*----------------
002600970605     d* Ds
002700970605     d*----------------
002800981013     d dstntbe       E DS                  EXTNAME(tntbe00f)                    *ds formato record
002900970610     d                                     INZ
003000970611     d                                     PREFIX(f_)
003100981013     d dstntber      E DS                  EXTNAME(tntbe00r)                    *ds formato record
003200970611     d                                     PREFIX(r_)
003300970610     d                                     INZ
003400970610     d dscmn           DS            80                                         *ds comando clp
003500970605     d   dmembro              45     54                                          -nome membro
003600970605     d wlbda8          DS                  INZ                                  *ds "XSRDA8"
003700970605     d  g08dat                        8  0                                       -data g/m/aa
003800970605     d  g08inv                        8  0                                       -data aa/m/g
003900970605     d  g08err                        1                                          -errore
004000970605     d  g08tgi                        5  0                                       -n°giorni data
004100970605     d kpjba         E DS                                                       *architettura
004200970605     d  libsys                92    101                                          -libreria sistema
004300970605     d  legfil               483    492                                          -libbreria file
004400970605     d  mbrfil               493    502                                          -membro file
004500970605     d*----------------
004600970605     d* Variabili riferite al data base
004700970605     d*----------------
004800981013     d ktbcod          S                   LIKE(r_tbecod)                       *lettura tntbe
004900981013     d ktbke1          S                   LIKE(r_tbeke1)                       *lettura tntbe
005000981013     d ktbke2          S                   LIKE(r_tbeke2)                       *lettura tntbe
005100981013     d ktblin          S                   LIKE(r_tbelin)                       *lettura tntbe
005200981013     d ktbsif          S                   LIKE(r_tbesif)                       *lettura tntbe
005300970605     d*----------------
005400970605     d* Variabili a programma
005500970605     d*----------------
005600970605     d lenght          S             15  5 INZ(80)                              *Lunghezza cmd CLP
005700970605     d comman          S             80                                         *valore cmd CLP
005800980828     d $rec            S              7  0                                      *rec ricevuti
005900970605     d wn14            S             14  0                                      *numerico 14
006000970605     d wn8             S              8  0                                      *numerico 8
006100970605     d datcor          S              8  0                                      *d.corrente aa/m/g
006200970605     c*--------------------------------------------------------------------------------------------*
006300970605     c* Main lines
006400970605     c*--------------------------------------------------------------------------------------------*
006500970605     c*
006600970605     c* verifica l'esistenza del membro di ricezione
006700970605     c                   EXSR      esimbr
006800970605     c*
006900970605     c* operazioni di ricezione
007000970605if  1c                   IF        NOT *in20                                    *no errori
007100970605     c                   EXSR      ricmbr
007200970605     c*
007300970605     c* disalloca il membro di ricezione
007400970605     c                   EXSR      dismbr
007500970605E   1c                   ENDIF
007600970605     c*
007700970605     c                   SETON                                        lr
007800970605     c*--------------------------------------------------------------------------------------------*
007900970605     c* esimbr - verifica l'esistenza del membro di ricezione
008000970605     c*--------------------------------------------------------------------------------------------*
008100970605     c     esimbr        BEGSR
008200970605     c*
008300981013     c* controlla esistenza membro di ricezione, se non esiste chiude il programma    tntbe
008400970605     c                   MOVEL(P)  cm1(1)        dscmn
008500970605     c                   MOVEL     mbrfil        dmembro
008600970605     c                   MOVEL(P)  dscmn         comman
008700970605     c                   CALL      'QCMDEXC'                            20
008800970605     c                   PARM                    comman
008900970605     c                   PARM                    lenght
009000970605     c*
009100981013     c* alloca il membro di ricezione, se non riesce chiude il programma              tntbe
009200970605if  1c                   IF        NOT *in20
009300970605     c                   MOVEL(P)  cm2(1)        dscmn
009400970605     c                   MOVEL     mbrfil        dmembro
009500970605     c                   MOVEL(P)  dscmn         comman
009600970605     c                   CALL      'QCMDEXC'                            20
009700970605     c                   PARM                    comman
009800970605     c                   PARM                    lenght
009900970605     c*
010000981013     c* esegue sovrapposizione membro di ricezione, se non riesce chiude il programma   tntbe
010100970605if  1c                   IF        NOT *in20
010200970605     c                   MOVEL(P)  cm3(1)        dscmn
010300970605     c                   MOVEL     mbrfil        dmembro
010400970605     c                   MOVEL(P)  dscmn         comman
010500970605     c                   CALL      'QCMDEXC'                            20
010600970605     c                   PARM                    comman
010700970605     c                   PARM                    lenght
010800970605e   2c                   ENDIF
010900970605e   1c                   ENDIF
011000980828     c*
011100970605     c                   ENDSR
011200970605     c*--------------------------------------------------------------------------------------------*
011300970605     c* ricmbr - operazioni di ricezione
011400970605     c*--------------------------------------------------------------------------------------------*
011500970605     c     ricmbr        BEGSR
011600970605     c*
011700970605     c* apre il file di ricezione
011800981013     c                   OPEN      tntbe00r
011900970605     c*
012000970605     c* legge il file di ricezione
012100981013     c                   READ      tntbe00r                               99
012200970605do  1c                   DOW       NOT *in99
012300970605     c*
012400970605     c* scrive il record fisico
012500970605     c                   EXSR      scrpfm
012600970605     c*
012700970605     c* cancella il record di ricezione
012800981013     c                   DELETE    tntber
012900970605     c                   ADD       1             $rec                           *n° record ricevuti
013000981013     c                   READ      tntbe00r                               99
013100970605e   1c                   ENDDO                                                  *fine file ricezione
013200970605     c*
013300970605     c* chiude il file di ricezione
013400981013     c                   CLOSE     tntbe00r
013500970605     c*
013600970605     c* aggiorna l'archivio di controllo delle ricezioni
013700970605     c                   EXSR      aggcmr
013800970605     c*
013900970605     c                   ENDSR
014000970605     c*--------------------------------------------------------------------------------------------*
014100970605     c* scrpfm - scrive il record fisico
014200970605     c*--------------------------------------------------------------------------------------------*
014300970605     c     scrpfm        BEGSR
014400970605     c*
014500970605     c* legge il record da aggiornare
014600981013     c                   move      r_tbecod      ktbcod
014700981013     c                   move      r_tbeke1      ktbke1
014800981013     c                   move      r_tbeke2      ktbke2
014900981013     c                   move      r_tbelin      ktblin
015000981013     c                   move      r_tbesif      ktbsif
015100980828     c*
015200981013     c     keytbe        CHAIN     tntbe01l                           98
015300981013     c*
015400981013     c* record esistente e da annullare, cancella il record fisico
015500981013if  1c                   IF        NOT *in98 AND r_TBEatb<>*blanks
015600981013     c                   DELETE    tntbe000
015700981013x   1c                   ELSE
015800970605     c*
015900970605     c* record inesistente o valido, riempe i campi del file fisico con quelli del record ricevuto
016000981013     c                   MOVEL     dstntber      dstntbe                        *formato record
016100981013     c                   MOVEL     'R'           f_tbeftr
016200981013     c                   Z-ADD     datcor        f_tbedtr
016300970605     c*
016400970605     c* inesistente, inserimento
016500970605if  2c                   IF        *in98
016600981013     c                   WRITE     tntbe000
016700970605     c*
016800970605     c* esistente, aggiornamento
016900970605x   2c                   ELSE
017000981013     c                   UPDATE    tntbe000
017100970605e   2c                   ENDIF
017200970605     c*
017300981013e   1c                   ENDIF
017400981013     c*
017500970605     c                   ENDSR
017600970605     c*--------------------------------------------------------------------------------------------*
017700970605     c* aggcmr - aggiorna l'archivio di controllo delle ricezioni
017800970605     c*--------------------------------------------------------------------------------------------*
017900970605     c     aggcmr        BEGSR
018000970605     c*
018100970605     c     legfil        CHAIN     cmric01l                           99
018200970605if  1c                   IF        NOT *in99                                    *esistente
018300970605     c                   Z-ADD     $rec          ricnr1                         *n° record ricevuti
018400970605     c                   MOVEL     '1'           ricfl2                         *archivio aggiornato
018500970605     c                   EXCEPT    updcmr
018600970605e   1c                   ENDIF
018700970605     c*
018800970605     c                   ENDSR
018900970605     c*--------------------------------------------------------------------------------------------*
019000970605     c* dismbr - disalloca il membro di ricezione
019100970605     c*--------------------------------------------------------------------------------------------*
019200970605     c     dismbr        BEGSR
019300970605     c*
019400970610     c* cancella la sovrascrittura del membro di ricezione
019500970605     c                   MOVEL(P)  cm4(1)        dscmn
019600970605     c                   MOVEL(P)  dscmn         comman
019700970605     c                   CALL      'QCMDEXC'                            20
019800970605     c                   PARM                    comman
019900970605     c                   PARM                    lenght
020000970605     c*
020100970605     c* disalloca il membro di ricezione
020200970605     c                   MOVEL(P)  cm5(1)        dscmn
020300970605     c                   MOVEL(P)  dscmn         comman
020400970605     c                   CALL      'QCMDEXC'                            20
020500970605     c                   PARM                    comman
020600970605     c                   PARM                    lenght
020700980828     c*
020800970605     c                   ENDSR
020900970605     c*--------------------------------------------------------------------------------------------*
021000970605     c* *inzsr - operazioni iniziali
021100970605     c*--------------------------------------------------------------------------------------------*
021200970605     c     *inzsr        BEGSR
021300970605     c*---
021400970605     c* ricevimento parametri
021500970605     c*---
021600970605     c     *ENTRY        PLIST
021700970605     c                   PARM                    kpjba
021800970605     c*---
021900970605     c* chiavi di lettura
022000970605     c*---
022100981013     c* di tntbe01l - completa
022200981013     c     keytbe        KLIST
022300981013     c                   KFLD                    ktbcod
022400981013     c                   KFLD                    ktbke1
022500981013     c                   KFLD                    ktbke2
022600981013     c                   KFLD                    ktblin
022700981013     c                   KFLD                    ktbsif
022800970605     c*
022900970605     c* calcola la data corrente
023000970605     c                   TIME                    wn14                           *ora(6) + data(8)
023100970605     c                   MOVE      wn14          wn8                            *data(8) in g/m/aa
023200970605     c                   Z-ADD     wn8           g08dat
023300970605     c                   Z-ADD     *zeros        g08inv
023400970605     c                   MOVEL     '0'           g08err
023500970605     c                   CALL      'XSRDA8'
023600970605     c                   PARM                    wlbda8
023700970605     c                   Z-ADD     g08inv        datcor                         *d.corrente aa/m/g
023800970605     c*
023900970605     c* se il membro da ricevere non è stato dichiarato, assume il primo del file
024000970605if  1c                   IF        mbrfil=*blanks
024100970605     c                   MOVEL     '*FIRST'      mbrfil
024200970605e   1c                   ENDIF
024300970605     c*
024400970605     c                   ENDSR
024500970605     o*--------------------------------------------------------------------------------------------*
024600970605     o* Aggiornamento file
024700970605     o*--------------------------------------------------------------------------------------------*
024800970605     ocmric000  E            updcmr
024900970605     o                       ricnr1
025000970605     o                       ricfl2
025100970605     o*--------------------------------------------------------------------------------------------*
025200970605**   cm1 - comandi
025300981013CHKOBJ OBJ(TNTBE00R) OBJTYPE(*FILE)     MBR(M234567890)
025400970605**   cm2 - comandi
025500981013ALCOBJ OBJ((TNTBE00R *FILE *EXCL            M234567890))
025600970605**   cm3 - comandi
025700981013OVRDBF FILE(TNTBE00R)                   MBR(M234567890)
025800970605**   cm4 - comandi
025900981013DLTOVR FILE(TNTBE00R)
026000970605**   cm5 - comandi
026100981013DLCOBJ OBJ((TNTBE00R *FILE *EXCL))
