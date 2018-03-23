000100020424     H*--------------------------------------------------------------------------------------------*
000200030108     H* Ricezione MOVIMENTI GABBIE/BANCALI
000300020424     H*--------------------------------------------------------------------------------------------*
000400020424     H DECEDIT('0,') DATEDIT(*DMY.)
000500020424     F*--------------------------------------------------------------------------------------------*
000600020424     F* Data base
000700020424     F*--------------------------------------------------------------------------------------------*
000800020424     Fcmric01l  UF   E           K DISK
000900030108     Fwfmgb00r  UF   E             DISK    USROPN
001000150224     Ftntbe01l  if   E           K DISK
001100030109     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
001200020424     D*--------------------------------------------------------------------------------------------*
001300020424     D* Data structure
001400020424     D*--------------------------------------------------------------------------------------------*
001500020424     D*----------------
001600020424     D* Schiere
001700020424     D*----------------
001800020424     D cm1             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
001900020424     D cm2             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002000020424     D cm3             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002100020424     D cm4             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002200020424     D cm5             S             80    DIM(1) CTDATA PERRCD(1)              *comando clp
002300020424     D*----------------
002400020424     D* Ds
002500020424     D*----------------
002600020424     D dscmn           DS            80                                         *ds comando clp
002700020424     D   dmembro              45     54                                          -nome membro
002800030108     D*
002900020424     D kpjba         E DS                                                       *architettura
003000020424     D  libsys                92    101                                          -libreria sistema
003100020424     D  legfil               483    492                                          -libbreria file
003200020424     D  mbrfil               493    502                                          -membro file
003300030108     D*
003400030108     D tisig0ds      E DS
003500150224     D tisigAds      E DS
003600150224     D dvpodeco      E DS
003700020424     D*----------------
003800020424     D* Variabili a programma
003900020424     D*----------------
004000150224     D savkpjbu        S                   like(kpjbu)
004100020424     D lenght          S             15  5 INZ(80)                              *Lunghezza cmd CLP
004200020424     D comman          S             80                                         *valore cmd CLP
004300020424     D $rec            S              7  0                                      *rec da trasmettere
004400020424     C*--------------------------------------------------------------------------------------------*
004500020424     C* Main lines
004600020424     C*--------------------------------------------------------------------------------------------*
004700020424     C*
004800020424     C* verifica l'esistenza del membro di ricezione
004900020424     C                   EXSR      esimbr
005000020424     C*
005100020424     C* operazioni di ricezione
005200020424     C                   IF        NOT *in20                                    *no errori
005300020424     C                   EXSR      ricmbr
005400020424     C*
005500020424     C* disalloca il membro di ricezione
005600020424     C                   EXSR      dismbr
005700020424     C                   ENDIF
005800020424     C*
005900020424     C                   SETON                                        lr
006000020424     C*--------------------------------------------------------------------------------------------*
006100020424     C* esimbr - verifica l'esistenza del membro di ricezione
006200020424     C*--------------------------------------------------------------------------------------------*
006300020424     C     esimbr        BEGSR
006400020424     C*
006500020424     C* controlla esistenza membro di ricezione, se non esiste chiude il programma
006600020424     C                   MOVEL(P)  cm1(1)        dscmn
006700020424     C                   MOVEL     mbrfil        dmembro
006800020424     C                   MOVEL(P)  dscmn         comman
006900020424     C                   CALL      'QCMDEXC'                            20
007000020424     C                   PARM                    comman
007100020424     C                   PARM                    lenght
007200020424     C*
007300020424     C* alloca il membro di ricezione, se non riesce chiude il programma
007400020424     C                   IF        NOT *in20
007500020424     C                   MOVEL(P)  cm2(1)        dscmn
007600020424     C                   MOVEL     mbrfil        dmembro
007700020424     C                   MOVEL(P)  dscmn         comman
007800020424     C                   CALL      'QCMDEXC'                            20
007900020424     C                   PARM                    comman
008000020424     C                   PARM                    lenght
008100020424     C*
008200020424     C* esegue sovrapposizione membro di ricezione, se non riesce chiude il programma
008300020424     C                   IF        NOT *in20
008400020424     C                   MOVEL(P)  cm3(1)        dscmn
008500020424     C                   MOVEL     mbrfil        dmembro
008600020424     C                   MOVEL(P)  dscmn         comman
008700020424     C                   CALL      'QCMDEXC'                            20
008800020424     C                   PARM                    comman
008900020424     C                   PARM                    lenght
009000020424     C                   ENDIF
009100020424     C                   ENDIF
009200020424     C*
009300020424     C                   ENDSR
009400020424     C*--------------------------------------------------------------------------------------------*
009500020424     C* ricmbr - operazioni di ricezione
009600020424     C*--------------------------------------------------------------------------------------------*
009700020424     C     ricmbr        BEGSR
009800020424     C*
009900020424     C* apre il file di ricezione
010000030108     C                   OPEN      wfmgb00r
010100020424     C*
010200020424     C* legge il file di ricezione
010300030108     C                   READ      wfmgb00r                               99
010400020424     C                   DOW       NOT *in99
010500020424     C*
010600020424     C* scrive il record fisico
010700020424     C                   EXSR      scrpfm
010800020424     C*
010900030108     C* se ok scrittura movimento cancella il record di ricezione
011000030108     C                   IF        OG0ERR = *blanks
011100030108     C                   DELETE    wfmgb00r
011200030109     C                   ELSE
011300030109     C                   EVAL      wfmFTR = 'E'
011400030109     C                   UPDATE    wfmgb000
011500031230     C*
011600031230     C* se nn ancora stampata, stampa testata
011700031230     C                   IF        *IN55 = *OFF
011800031230     C                   EXCEPT    TES
011900031230     C                   SETON                                        55
012000031230     C                   ENDIF
012100031230     C*
012200030109     C* stampa dettaglio errore
012300030109     C                   EXCEPT    DET
012400030108     C                   ENDIF
012500020424     C                   ADD       1             $rec                           *n° record ricevuti
012600030108     C                   READ      wfmgb00r                               99
012700020424     C                   ENDDO                                                  *fine file ricezione
012800020424     C*
012900020424     C* chiude il file di ricezione
013000030108     C                   CLOSE     wfmgb00r
013100020424     C*
013200020424     C* aggiorna l'archivio di controllo delle ricezioni
013300020424     C                   EXSR      aggcmr
013400020424     C*
013500020424     C                   ENDSR
013600020424     C*--------------------------------------------------------------------------------------------*
013700020424     C* scrpfm - scrive il record fisico
013800020424     C*--------------------------------------------------------------------------------------------*
013900020424     C     scrpfm        BEGSR
014000030108     C*
014100030108     C* Imposta la DS di procedura da passare al "driver" x la scrittura del movimento
014200030108     C                   CLEAR                   TISIG0DS
014300030108     C                   Z-ADD     wfmDAT        IG0DAT
014400030108     C                   Z-ADD     wfmORA        IG0ORA
014500030108     C                   MOVEL     wfmUTE        IG0UTE
014600030108     C                   Z-ADD     wfmPOU        IG0POU
014700030108     C                   MOVEL     wfmCAU        IG0CAU
014800030108     C                   MOVEL     wfmSOG        IG0SOG
014900030108     C                   MOVEL     wfmCOD        IG0COD
015000030108     C                   Z-ADD     wfmQTA        IG0QTA
015100030108     C                   MOVEL     wfmRDO        IG0RDO
015200030108     C                   Z-ADD     wfmDDO        IG0DDO
015300030108     C                   MOVEL     wfmSMC        IG0SMC
015400150224     c                   clear                   tisigads
015500150224     c                   movel     kpjbu         savkpjbu
015600150305     c                   if        ig0dat >= §vpobanc
015700150224     c                   move      IG0sog        IGAKSC
015800150224     c                   move      IG0dat        IGADAT
015900150224     c                   move      IG0cod        IGACOD
016000150302     c                   movel     IG0cod        IGAtsc
016100150224     c                   movel     tisigads      kpjbu
016200150224     c                   call      'TISIGAR'
016300150224     c                   parm                    kpjba
016400150224     c                   movel     kpjbu         tisigads
016500150224     c                   movel     savkpjbu      kpjbu
016600150224     c                   if        ogaesito <> *blank
016700150224     c                   leavesr
016800150224     c                   endif
016900150224     c                   move      ogaksu        ig0sog
017000150224     c                   endif
017100030108     C*
017200030108     C* Lancio del "driver"
017300030206     C                   CALL(e)   'TISIG5R'
017400150227     C                   PARM                    kpjba
017500030108     C                   PARM                    TISIG0DS
017600030206     C*
017700030206     C* Se errore in chiamata "driver" gestisco come se restituito errore dal "driver" stesso
017800030206     C                   IF        %error
017900030206     C                   EVAL      OG0ERR = 'E'
018000030206     C                   EVAL      OG0MSG = 'ERRORE IN CHIAMAATA DRIVER'
018100030206     C                   ENDIF
018200020424     C*
018300020424     C                   ENDSR
018400020424     C*--------------------------------------------------------------------------------------------*
018500020424     C* aggcmr - aggiorna l'archivio di controllo delle ricezioni
018600020424     C*--------------------------------------------------------------------------------------------*
018700020424     C     aggcmr        BEGSR
018800020424     C*
018900020424     C     legfil        CHAIN     cmric01l                           99
019000020424     C                   IF        NOT *in99                                    *esistente
019100020424     C                   Z-ADD     $rec          ricnr1                         *n° record ricevuti
019200020424     C                   MOVEL     '1'           ricfl2                         *archivio aggiornato
019300020424     C                   EXCEPT    updcmr
019400020424     C                   ENDIF
019500020424     C*
019600020424     C                   ENDSR
019700020424     C*--------------------------------------------------------------------------------------------*
019800020424     C* dismbr - disalloca il membro di ricezione
019900020424     C*--------------------------------------------------------------------------------------------*
020000020424     C     dismbr        BEGSR
020100020424     C*
020200020424     C* cancella la sobvrascrittura del membro di ricezione
020300020424     C                   MOVEL(P)  cm4(1)        dscmn
020400020424     C                   MOVEL(P)  dscmn         comman
020500020424     C                   CALL      'QCMDEXC'                            20
020600020424     C                   PARM                    comman
020700020424     C                   PARM                    lenght
020800020424     C*
020900020424     C* disalloca il membro di ricezione
021000020424     C                   MOVEL(P)  cm5(1)        dscmn
021100020424     C                   MOVEL(P)  dscmn         comman
021200020424     C                   CALL      'QCMDEXC'                            20
021300020424     C                   PARM                    comman
021400020424     C                   PARM                    lenght
021500020424     C*
021600020424     C                   ENDSR
021700020424     C*--------------------------------------------------------------------------------------------*
021800020424     C* *inzsr - operazioni iniziali
021900020424     C*--------------------------------------------------------------------------------------------*
022000020424     C     *inzsr        BEGSR
022100030108     C*
022200030108     C* Ricevimento parametri
022300020424     C     *ENTRY        PLIST
022400020424     C                   PARM                    kpjba
022500020424     C*
022600030108     C* Se il membro da ricevere non è stato dichiarato, assume il primo del file
022700020424     C                   IF        mbrfil=*blanks
022800020424     C                   MOVEL     '*FIRST'      mbrfil
022900020424     C                   ENDIF
023000150224     c     ktbe          klist
023100150224     c                   kfld                    TBECOD
023200150224     c                   kfld                    TBEKE1
023300150224     c                   move      'VPO'         tbecod
023400150224     c                   movel(p)  'DECO'        tbeke1
023500150224     c                   clear                   dvpodeco
023600150224     c     ktbe          chain     tntbe01l
023700150224     c                   if        %found(tntbe01l)
023800150224     c                   movel     tbeuni        dvpodeco
023900150224     c                   endif
024000020424     C*
024100020424     C                   ENDSR
024200020424     O*--------------------------------------------------------------------------------------------*
024300020424     O* Aggiornamento file
024400020424     O*--------------------------------------------------------------------------------------------*
024500020424     Ocmric000  E            updcmr
024600020424     O                       ricnr1
024700020424     O                       ricfl2
024800020424     O*--------------------------------------------------------------------------------------------*
024900030109     OQSYSPRT   E            TES               2
025000030109     O                       KNSIF               20
025100030109     O                                           30 'TISIG6R '
025200030109     O                                           62 '*** ERRORI MOV. GABBIE ***'
025300030109     O                       UDATE               95 '  /  /  '
025400030109     O                                          128 'PAG.'
025500030109     O                       PAGE          Z    198
025600030109     O*------------------------------------
025700030109     O          E            DET         1
025800030109     O                       LEGFIL              11
025900030109     O                       MBRFIL              22
026000030109     O                       IG0DAT              33 '    /  /  '
026100030109     O                       IG0ORA              42 '  :  :  '
026200030109     O                       IG0UTE              53
026300030109     O                       IG0POU              57
026400030109     O                       IG0PGM              68
026500030109     O                       IG0CAU              73
026600030109     O                       IG0SOG              82
026700030109     O                       IG0COD              98
026800030109     O                       IG0SMC             107
026900030109     O                       OG0MSG             188
027000030109     O*--------------------------------------------------------------------------------------------*
027100970605**   cm1 - comandi
027200030108CHKOBJ OBJ(WFMGB00R) OBJTYPE(*FILE)     MBR(M234567890)
027300970605**   cm2 - comandi
027400030108ALCOBJ OBJ((WFMGB00R *FILE *EXCL            M234567890))
027500970605**   cm3 - comandi
027600030108OVRDBF FILE(WFMGB00R)                   MBR(M234567890)
027700970605**   cm4 - comandi
027800030108DLTOVR FILE(WFMGB00R)
027900970605**   cm5 - comandi
028000030108DLCOBJ OBJ((WFMGB00R *FILE *EXCL))
