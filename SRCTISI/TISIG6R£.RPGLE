000100020424     H*--------------------------------------------------------------------------------------------*
000200030108     H* Ricezione MOVIMENTI GABBIE/BANCALI
000300020424     H*--------------------------------------------------------------------------------------------*
000400020424     H DECEDIT('0,') DATEDIT(*DMY.)
000500020424     F*--------------------------------------------------------------------------------------------*
000600020424     F* Data base
000700020424     F*--------------------------------------------------------------------------------------------*
000800020424     Fcmric01l  UF   E           K DISK
000900030108     Fwfmgb00r  UF   E             DISK    USROPN
001000030109     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
001100020424     D*--------------------------------------------------------------------------------------------*
001200020424     D* Data structure
001300020424     D*--------------------------------------------------------------------------------------------*
001400020424     D*----------------
001500020424     D* Schiere
001600020424     D*----------------
001700020424     D cm1             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
001800020424     D cm2             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
001900020424     D cm3             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002000020424     D cm4             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002100020424     D cm5             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002200020424     D*----------------
002300020424     D* Ds
002400020424     D*----------------
002500020424     D dscmn           DS            80                                         *ds comando clp
002600020424     D   dmembro              45     54                                          -nome membro
002700030108     D*
002800020424     D kpjba         E DS                                                       *architettura
002900020424     D  libsys                92    101                                          -libreria sistema
003000020424     D  legfil               483    492                                          -libbreria file
003100020424     D  mbrfil               493    502                                          -membro file
003200030108     D*
003300030108     D tisig0ds      E DS
003400020424     D*----------------
003500020424     D* Variabili a programma
003600020424     D*----------------
003700020424     D lenght          S             15  5 INZ(80)                              *Lunghezza cmd CLP
003800020424     D comman          S             80                                         *valore cmd CLP
003900020424     D $rec            S              7  0                                      *rec da trasmettere
004000020424     C*--------------------------------------------------------------------------------------------*
004100020424     C* Main lines
004200020424     C*--------------------------------------------------------------------------------------------*
004300020424     C*
004400020424     C* verifica l'esistenza del membro di ricezione
004500020424     C                   EXSR      esimbr
004600020424     C*
004700020424     C* operazioni di ricezione
004800020424     C                   IF        NOT *in20                                    *no errori
004900020424     C                   EXSR      ricmbr
005000020424     C*
005100020424     C* disalloca il membro di ricezione
005200020424     C                   EXSR      dismbr
005300020424     C                   ENDIF
005400020424     C*
005500020424     C                   SETON                                        lr
005600020424     C*--------------------------------------------------------------------------------------------*
005700020424     C* esimbr - verifica l'esistenza del membro di ricezione
005800020424     C*--------------------------------------------------------------------------------------------*
005900020424     C     esimbr        BEGSR
006000020424     C*
006100020424     C* controlla esistenza membro di ricezione, se non esiste chiude il programma
006200020424     C                   MOVEL(P)  cm1(1)        dscmn
006300020424     C                   MOVEL     mbrfil        dmembro
006400020424     C                   MOVEL(P)  dscmn         comman
006500020424     C                   CALL      'QCMDEXC'                            20
006600020424     C                   PARM                    comman
006700020424     C                   PARM                    lenght
006800020424     C*
006900020424     C* alloca il membro di ricezione, se non riesce chiude il programma
007000020424     C                   IF        NOT *in20
007100020424     C                   MOVEL(P)  cm2(1)        dscmn
007200020424     C                   MOVEL     mbrfil        dmembro
007300020424     C                   MOVEL(P)  dscmn         comman
007400020424     C                   CALL      'QCMDEXC'                            20
007500020424     C                   PARM                    comman
007600020424     C                   PARM                    lenght
007700020424     C*
007800020424     C* esegue sovrapposizione membro di ricezione, se non riesce chiude il programma
007900020424     C                   IF        NOT *in20
008000020424     C                   MOVEL(P)  cm3(1)        dscmn
008100020424     C                   MOVEL     mbrfil        dmembro
008200020424     C                   MOVEL(P)  dscmn         comman
008300020424     C                   CALL      'QCMDEXC'                            20
008400020424     C                   PARM                    comman
008500020424     C                   PARM                    lenght
008600020424     C                   ENDIF
008700020424     C                   ENDIF
008800020424     C*
008900020424     C                   ENDSR
009000020424     C*--------------------------------------------------------------------------------------------*
009100020424     C* ricmbr - operazioni di ricezione
009200020424     C*--------------------------------------------------------------------------------------------*
009300020424     C     ricmbr        BEGSR
009400020424     C*
009500020424     C* apre il file di ricezione
009600030108     C                   OPEN      wfmgb00r
009700020424     C*
009800020424     C* legge il file di ricezione
009900030108     C                   READ      wfmgb00r                               99
010000020424     C                   DOW       NOT *in99
010100020424     C*
010200020424     C* scrive il record fisico
010300020424     C                   EXSR      scrpfm
010400020424     C*
010500030108     C* se ok scrittura movimento cancella il record di ricezione
010600030108     C                   IF        OG0ERR = *blanks
010700030108     C                   DELETE    wfmgb00r
010800030109     C                   ELSE
010900030109     C                   EVAL      wfmFTR = 'E'
011000030109     C                   UPDATE    wfmgb000
011100031230     C*
011200031230     C* se nn ancora stampata, stampa testata
011300031230     C                   IF        *IN55 = *OFF
011400031230     C                   EXCEPT    TES
011500031230     C                   SETON                                        55
011600031230     C                   ENDIF
011700031230     C*
011800030109     C* stampa dettaglio errore
011900030109     C                   EXCEPT    DET
012000030108     C                   ENDIF
012100020424     C                   ADD       1             $rec                           *n° record ricevuti
012200030108     C                   READ      wfmgb00r                               99
012300020424     C                   ENDDO                                                  *fine file ricezione
012400020424     C*
012500020424     C* chiude il file di ricezione
012600030108     C                   CLOSE     wfmgb00r
012700020424     C*
012800020424     C* aggiorna l'archivio di controllo delle ricezioni
012900020424     C                   EXSR      aggcmr
013000020424     C*
013100020424     C                   ENDSR
013200020424     C*--------------------------------------------------------------------------------------------*
013300020424     C* scrpfm - scrive il record fisico
013400020424     C*--------------------------------------------------------------------------------------------*
013500020424     C     scrpfm        BEGSR
013600030108     C*
013700030108     C* Imposta la DS di procedura da passare al "driver" x la scrittura del movimento
013800030108     C                   CLEAR                   TISIG0DS
013900030108     C                   Z-ADD     wfmDAT        IG0DAT
014000030108     C                   Z-ADD     wfmORA        IG0ORA
014100030108     C                   MOVEL     wfmUTE        IG0UTE
014200030108     C                   Z-ADD     wfmPOU        IG0POU
014300030108     C                   MOVEL     wfmCAU        IG0CAU
014400030108     C                   MOVEL     wfmSOG        IG0SOG
014500030108     C                   MOVEL     wfmCOD        IG0COD
014600030108     C                   Z-ADD     wfmQTA        IG0QTA
014700030108     C                   MOVEL     wfmRDO        IG0RDO
014800030108     C                   Z-ADD     wfmDDO        IG0DDO
014900030108     C                   MOVEL     wfmSMC        IG0SMC
015000030108     C*
015100030108     C* Lancio del "driver"
015200030206     C                   CALL(e)   'TISIG5R'
015300030108     C                   PARM                    TISIG0DS
015400030206     C*
015500030206     C* Se errore in chiamata "driver" gestisco come se restituito errore dal "driver" stesso
015600030206     C                   IF        %error
015700030206     C                   EVAL      OG0ERR = 'E'
015800030206     C                   EVAL      OG0MSG = 'ERRORE IN CHIAMAATA DRIVER'
015900030206     C                   ENDIF
016000020424     C*
016100020424     C                   ENDSR
016200020424     C*--------------------------------------------------------------------------------------------*
016300020424     C* aggcmr - aggiorna l'archivio di controllo delle ricezioni
016400020424     C*--------------------------------------------------------------------------------------------*
016500020424     C     aggcmr        BEGSR
016600020424     C*
016700020424     C     legfil        CHAIN     cmric01l                           99
016800020424     C                   IF        NOT *in99                                    *esistente
016900020424     C                   Z-ADD     $rec          ricnr1                         *n° record ricevuti
017000020424     C                   MOVEL     '1'           ricfl2                         *archivio aggiornato
017100020424     C                   EXCEPT    updcmr
017200020424     C                   ENDIF
017300020424     C*
017400020424     C                   ENDSR
017500020424     C*--------------------------------------------------------------------------------------------*
017600020424     C* dismbr - disalloca il membro di ricezione
017700020424     C*--------------------------------------------------------------------------------------------*
017800020424     C     dismbr        BEGSR
017900020424     C*
018000020424     C* cancella la sobvrascrittura del membro di ricezione
018100020424     C                   MOVEL(P)  cm4(1)        dscmn
018200020424     C                   MOVEL(P)  dscmn         comman
018300020424     C                   CALL      'QCMDEXC'                            20
018400020424     C                   PARM                    comman
018500020424     C                   PARM                    lenght
018600020424     C*
018700020424     C* disalloca il membro di ricezione
018800020424     C                   MOVEL(P)  cm5(1)        dscmn
018900020424     C                   MOVEL(P)  dscmn         comman
019000020424     C                   CALL      'QCMDEXC'                            20
019100020424     C                   PARM                    comman
019200020424     C                   PARM                    lenght
019300020424     C*
019400020424     C                   ENDSR
019500020424     C*--------------------------------------------------------------------------------------------*
019600020424     C* *inzsr - operazioni iniziali
019700020424     C*--------------------------------------------------------------------------------------------*
019800020424     C     *inzsr        BEGSR
019900030108     C*
020000030108     C* Ricevimento parametri
020100020424     C     *ENTRY        PLIST
020200020424     C                   PARM                    kpjba
020300020424     C*
020400030108     C* Se il membro da ricevere non è stato dichiarato, assume il primo del file
020500020424     C                   IF        mbrfil=*blanks
020600020424     C                   MOVEL     '*FIRST'      mbrfil
020700020424     C                   ENDIF
020800020424     C*
020900020424     C                   ENDSR
021000020424     O*--------------------------------------------------------------------------------------------*
021100020424     O* Aggiornamento file
021200020424     O*--------------------------------------------------------------------------------------------*
021300020424     Ocmric000  E            updcmr
021400020424     O                       ricnr1
021500020424     O                       ricfl2
021600020424     O*--------------------------------------------------------------------------------------------*
021700030109     OQSYSPRT   E            TES               2
021800030109     O                       KNSIF               20
021900030109     O                                           30 'TISIG6R '
022000030109     O                                           62 '*** ERRORI MOV. GABBIE ***'
022100030109     O                       UDATE               95 '  /  /  '
022200030109     O                                          128 'PAG.'
022300030109     O                       PAGE          Z    198
022400030109     O*------------------------------------
022500030109     O          E            DET         1
022600030109     O                       LEGFIL              11
022700030109     O                       MBRFIL              22
022800030109     O                       IG0DAT              33 '    /  /  '
022900030109     O                       IG0ORA              42 '  :  :  '
023000030109     O                       IG0UTE              53
023100030109     O                       IG0POU              57
023200030109     O                       IG0PGM              68
023300030109     O                       IG0CAU              73
023400030109     O                       IG0SOG              82
023500030109     O                       IG0COD              98
023600030109     O                       IG0SMC             107
023700030109     O                       OG0MSG             188
023800030109     O*--------------------------------------------------------------------------------------------*
023900970605**   cm1 - comandi
024000030108CHKOBJ OBJ(WFMGB00R) OBJTYPE(*FILE)     MBR(M234567890)
024100970605**   cm2 - comandi
024200030108ALCOBJ OBJ((WFMGB00R *FILE *EXCL            M234567890))
024300970605**   cm3 - comandi
024400030108OVRDBF FILE(WFMGB00R)                   MBR(M234567890)
024500970605**   cm4 - comandi
024600030108DLTOVR FILE(WFMGB00R)
024700970605**   cm5 - comandi
024800030108DLCOBJ OBJ((WFMGB00R *FILE *EXCL))
