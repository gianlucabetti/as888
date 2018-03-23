000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA RITARDI TRAINI - ESEGUE
000300000000     H* NB. NON ELABORABILE A CAVALLO D'ANNO
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900960927     FAZORG01L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001100000000     FTNSS88P   O    E             PRINTER OFLIND(*IN01)
001200960927     F*------------
001300960927     F* FILE DI LAVORO
001400960927     F*------------
001500960927     FWFRIT01L  IF   E           K DISK
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D*------------------
002000000000     D* DECODIFICA RIGHE DI STAMPA
002100000000     D*------------------
002200000000     D DRI             S             30    DIM(20) CTDATA PERRCD(1)
002300000000     D*------------------
002400000000     D* DECODIFICA MESI DELL'ANNO
002500000000     D*------------------
002600000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
002700000000     D*------------------
002800000000     D* PERIDO DA ELABORARE
002900000000     D*------------------
003000990608     D ANN             S              4  0 DIM(12)
003100000000     D MES             S              2  0 DIM(12)
003200000000     D*------------------
003300000000     D* ORGANIGRAMMA
003400000000     D*------------------
003500971124     D ORG             S              3  0 DIM(500)                             *FILAILE
003600971124     D DORG            S             20    DIM(500)                             *DESCRIZIONE
003700020627     D POS             S              3    DIM(500)                             *NETWORK POSTE
003800000706     D DIV             S              1    DIM(500)                             *DIVISIONE
003900971124     D DDIV            S             20    DIM(500)                             *DESCRIZIONE
004000000000     D*------------------
004100000000     D* SCHIERE MULTIPLE
004200000000     D*------------------
004300000000     D* TERMINAL
004400000000     D* DIVISIONE
004500000000     D* GENERALE
004600000000     D* DI STAMPA
004700000000     D*------------------
004800000000     D* SCHIERE
004900000000     D*------------------
005000000000     D* LAVORO -> 12 MESI + 3 DECADE + 1 TOTALE
005100000000     D* DI LAVORO
005200000000     D $PAR            S             13  0 DIM(16)                              *TOTALE PARTENZE
005300000000     D $RIT            S             13  0 DIM(16)                              *TOTALE RITARDI
005400990817     D $FIL            S             13  0 DIM(16)                              *FILIALE ARR RITARDO
005500000000     D $AUT            S             13  0 DIM(16)                              *AUTISTA
005600000000     D $CFM            S             13  0 DIM(16)                              *CAUSA FORZA MAGG
005700000000     D $SED            S             13  0 DIM(16)                              *SEDE TRAINI
005800000000     D $TRS            S             13  0 DIM(16)                              *TRANSITO
005900990817     D $FIO            S             13  0 DIM(16)                              *FILIALE ARR ORARIO
006000000000     D* DI PASSAGGIO
006100000000     D PV1             S             13  0 DIM(16)                              *VALORE UNO
006200000000     D PP1             S              6  3 DIM(16)                              *PERCENTUALE UNO
006300000000     D*------------------------------------------------------------------------*
006400000000     D* INPUT
006500000000     D*------------------------------------------------------------------------*
006600000000     D*------------------
006700000000     D* NUMERO DI BYTE PACKED
006800000000     D*------------------
006900000000     D* ES. (13:2)+1 = 7,5 ; (7X12) = 84
007000000000     D*------------------
007100000000     D* DS MULTIPLA TERMINAL              -3 DECADI-
007200000000     D*------------------
007300000000     D TDEC            DS                  OCCURS(3) INZ
007400000000     D  TPAR                   1     84P 0
007500000000     D                                     DIM(12)                              *TOTALE PARTENZE
007600000000     D  TRIT                  85    168P 0
007700000000     D                                     DIM(12)                              *TOTALE RITARDI
007800000000     D  TFIL                 169    252P 0
007900990817     D                                     DIM(12)                              *FILIALE ARR RITARDO
008000000000     D  TAUT                 253    336P 0
008100000000     D                                     DIM(12)                              *AUTISTA
008200000000     D  TCFM                 337    420P 0
008300000000     D                                     DIM(12)                              *CAUSA FORZA MAGG
008400000000     D  TSED                 421    504P 0
008500000000     D                                     DIM(12)                              *SEDE TRAINI
008600000000     D  TTRS                 505    588P 0
008700000000     D                                     DIM(12)                              *TRANSITO
008800990817     D  TFIO                 589    672P 0
008900990817     D                                     DIM(12)                              *FILIALE ARR ORARIO
009000000000     D*------------------
009100000000     D* DS MULTIPLA DIVISIONE             -3 DECADI-
009200000000     D*------------------
009300000000     D DDEC            DS                  OCCURS(3) INZ
009400000000     D  DPAR                   1     84P 0
009500000000     D                                     DIM(12)                              *TOTALE PARTENZE
009600000000     D  DRIT                  85    168P 0
009700000000     D                                     DIM(12)                              *TOTALE RITARDI
009800000000     D  DFIL                 169    252P 0
009900990817     D                                     DIM(12)                              *FILIALE ARR RITARDO
010000000000     D  DAUT                 253    336P 0
010100000000     D                                     DIM(12)                              *AUTISTA
010200000000     D  DCFM                 337    420P 0
010300000000     D                                     DIM(12)                              *CAUSA FORZA MAGG
010400000000     D  DSED                 421    504P 0
010500000000     D                                     DIM(12)                              *SEDE TRAINI
010600000000     D  DTRS                 505    588P 0
010700000000     D                                     DIM(12)                              *TRANSITO
010800990817     D  DFIO                 589    672P 0
010900990817     D                                     DIM(12)                              *FILIALE ARR ORARIO
011000000000     D*------------------
011100000000     D* DS MULTIPLA GENERALE              -3 DECADI-
011200000000     D*------------------
011300000000     D GDEC            DS                  OCCURS(3) INZ
011400000000     D  GPAR                   1     84P 0
011500000000     D                                     DIM(12)                              *TOTALE PARTENZE
011600000000     D  GRIT                  85    168P 0
011700000000     D                                     DIM(12)                              *TOTALE RITARDI
011800000000     D  GFIL                 169    252P 0
011900990817     D                                     DIM(12)                              *FILIALE ARR RITARDO
012000000000     D  GAUT                 253    336P 0
012100000000     D                                     DIM(12)                              *AUTISTA
012200000000     D  GCFM                 337    420P 0
012300000000     D                                     DIM(12)                              *CAUSA FORZA MAGG
012400000000     D  GSED                 421    504P 0
012500000000     D                                     DIM(12)                              *SEDE TRAINI
012600000000     D  GTRS                 505    588P 0
012700000000     D                                     DIM(12)                              *TRANSITO
012800990817     D  GFIO                 589    672P 0
012900990817     D                                     DIM(12)                              *FILIALE ARR ORARIO
013000000000     D*------------------
013100000000     D* DS MULTIPLA STAMPA                -3 DECADI-
013200000000     D*------------------
013300000000     D SDEC            DS                  OCCURS(3) INZ
013400000000     D  SPAR                   1     84P 0
013500000000     D                                     DIM(12)                              *TOTALE PARTENZE
013600000000     D  SRIT                  85    168P 0
013700000000     D                                     DIM(12)                              *TOTALE RITARDI
013800000000     D  SFIL                 169    252P 0
013900990817     D                                     DIM(12)                              *FILIALE ARR RITARDO
014000000000     D  SAUT                 253    336P 0
014100000000     D                                     DIM(12)                              *AUTISTA
014200000000     D  SCFM                 337    420P 0
014300000000     D                                     DIM(12)                              *CAUSA FORZA MAGG
014400000000     D  SSED                 421    504P 0
014500000000     D                                     DIM(12)                              *SEDE TRAINI
014600000000     D  STRS                 505    588P 0
014700000000     D                                     DIM(12)                              *TRANSITO
014800990817     D  SFIO                 589    672P 0
014900990817     D                                     DIM(12)                              *FILIALE ARR ORARIO
015000000000     D*------------------
015100000000     D* ARCHITETTURA
015200000000     D*------------------
015300000000     D KPJBA         E DS
015400960927     D  PROFIL                 1     10
015500000000     D*------------------
015600000000     D* DS PARAMETRI LANCIO
015700000000     D*------------------
015800000000     D PARAM           DS
015900990608     D  PARDAT                 1      6  0
016000990608     D  PARAAI                 1      4  0
016100990608     D  PARMMI                 5      6  0
016200990608     D  PARDA2                 7     12  0
016300990608     D  PARAAF                 7     10  0
016400990608     D  PARMMF                11     12  0
016500990608     D  PARCRE                13     13
016600000706     D  PARPOS                14     14
016700000706     D  PARTAD                15     15
016800000000     D*------------------
016900000000     D* DS SCOMPOSIZIONE DATA TRAINO
017000000000     D*------------------
017100000000     D                 DS
017200960927     D  WFLANN                 1      4  0
017300960927     D  WFLMES                 5      6  0
017400960927     D  WFLDAT                 1      6  0
017500000706     D*------------------
017600000706     D* DS LETTURA CAMPO ORGDE3 DI AZORG00F
017700000706     D*------------------
017800000706     D OG143         E DS
017900000000     C*------------------------------------------------------------------------*
018000960927     C* MAIN LINE
018100000000     C*------------------------------------------------------------------------*
018200000000     C*
018300000000     C* STAMPA LA PAGINA DEI PARAMETRI DI LANCIO
018400000000     C                   EXSR      STAPAR
018500000000     C*
018600960927     C* POSIZIONAMENTO E PRIMA LETTURA WORKFILE
018700000000     C                   EXSR      SETFIL
018800960927     C*
018900960927     C* CICLO FINO A FINE FILE
019000960927DO  1C     $FINE         DOWEQ     '0'
019100960927     C*
019200960927     C* OPERAZIONI PER NUOVA DIVISIONE
019300960927     C                   EXSR      NEWDIV
019400960927     C*
019500960927     C* CICLO FINO A ROTTURA DIVISIONE
019600960927DO  2C     $FINE         DOWEQ     '0'
019700960927     C     DEPDIV        ANDEQ     WFLDIV
019800000000     C*
019900000000     C* OPERAZIONI PER NUOVA FILIALE
020000000000     C                   EXSR      NEWTFP
020100000000     C*
020200000000     C* CICLO FINO A ROTTURA FILIALE
020300960927DO  3C     $FINE         DOWEQ     '0'
020400960927     C     DEPDIV        ANDEQ     WFLDIV
020500960927     C     DEPTFP        ANDEQ     WFLTFP
020600000000     C*
020700000000     C* OPERAZIONI PER NUOVO MESE
020800000000     C                   EXSR      NEWMES
020900000000     C*
021000000000     C* CICLO FINO A ROTTURA MESE
021100960927DO  4C     $FINE         DOWEQ     '0'
021200960927     C     DEPDIV        ANDEQ     WFLDIV
021300960927     C     DEPTFP        ANDEQ     WFLTFP
021400960927     C     DEPMES        ANDEQ     WFLMES
021500000000     C*
021600000000     C* OPERAZIONI PER NUOVA DECADE
021700000000     C                   EXSR      NEWDEC
021800000000     C*
021900000000     C* CICLO FINO A ROTTURA DECADE
022000960927DO  5C     $FINE         DOWEQ     '0'
022100960927     C     DEPDIV        ANDEQ     WFLDIV
022200960927     C     DEPTFP        ANDEQ     WFLTFP
022300960927     C     DEPMES        ANDEQ     WFLMES
022400960927     C     DEPDEC        ANDEQ     WFLDEC
022500000000     C*
022600000000     C* MEMORIZZA I DATI DEI SALDI
022700000000     C                   EXSR      MEMSAL
022800000000     C*
022900000000     C* LEGGE IL RECORD SUCCESSIVO
023000000000     C                   EXSR      LETFIL
023100960927E   5C                   ENDDO                                                  *ROTTURA DECADE
023200960927E   4C                   ENDDO                                                  *ROTTURA MESE
023300960927E   3C                   ENDDO                                                  *ROTTURA FILIALE
023400000000     C*
023500000000     C* STAMPA TOTALI PER FILIALE
023600000000     C                   EXSR      STATFP
023700000000E   2C                   ENDDO                                                  *ROTTURA DIVISIONE
023800000000     C*
023900960903     C* STAMPA TOTALI PER DIVISIONE
024000960927IF  2C     PARTAD        IFEQ      'D'                                          *SE TOT X DIVISIONE
024100000000     C                   EXSR      STADIV
024200960927E   2C                   ENDIF
024300960927E   1C                   ENDDO                                                  *FINE FILE
024400000000     C*
024500000000     C* STAMPA TOTALI GENERALI
024600960903IF  1C     PARTAD        IFEQ      'D'                                          *SE TOT X DIVISIONE
024700000000     C                   EXSR      STAGEN
024800960903X   1C                   ELSE
024900960903     C                   WRITE     SS88RI                                       *STAMPA RIGA CHIUSUR
025000960903E   1C                   ENDIF
025100000000     C*
025200000000     C* STAMPA RIGA DI FINE PROSPETTO
025300000000     C                   WRITE     SS88FI
025400000000     C*
025500000000     C                   SETON                                        LR
025600000000     C*------------------------------------------------------------------------*
025700000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
025800000000     C*------------------------------------------------------------------------*
025900000000     C     SETFIL        BEGSR
026000000000     C*
026100960927     C     *LOVAL        SETLL     WFRIT01L                           99        *FINE ARCHIVIO
026200000000IF  1C     *IN99         IFEQ      *ON
026300000000     C                   MOVEL     '1'           $FINE
026400000000X   1C                   ELSE
026500000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
026600000000E   1C                   ENDIF
026700000000     C*
026800000000     C                   ENDSR
026900000000     C*------------------------------------------------------------------------*
027000000000     C* LETFIL - LETTURA PROSSIMO RECORD
027100000000     C*------------------------------------------------------------------------*
027200000000     C     LETFIL        BEGSR
027300000000     C*
027400000000     C* LEGGE FINO A:
027500000000     C*     - FINE FILE
027600000000     C*     - TROVATO RECORD VALIDO
027700000000     C                   MOVEL     'N'           WRECOK
027800000000DO  1C     $FINE         DOWEQ     '0'
027900000000     C     WRECOK        ANDEQ     'N'
028000000000     C*
028100960927     C                   READ      WFRIT01L                               99
028200960927     C*
028300000000IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
028400000000     C                   MOVEL     '1'           $FINE
028500000000X   2C                   ELSE
028600000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
028700000000E   2C                   ENDIF
028800000000E   1C                   ENDDO
028900000000     C*
029000000000     C                   ENDSR
029100000000     C*------------------------------------------------------------------------*
029200000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
029300000000     C*------------------------------------------------------------------------*
029400000000     C     CHKREC        BEGSR
029500000000     C*
029600000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
029700000000     C*
029800000000     C* PERIODO NON RICHIESTO, ESCLUDE
029900990608IF  1C     WFLDAT        IFGE      PARDAT
030000990608     C     WFLDAT        ANDLE     PARDA2
030100000000X   1C                   ELSE
030200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
030300000000E   1C                   ENDIF
030400000000     C*
030500000710     C* LANCIO NO POSTE, ESCLUDE I TERMINAL POSTE
030600000706IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
030700000706     C                   SETOFF                                           28
030800000706     C                   Z-ADD     1             I
030900000706     C     WFLTFP        LOOKUP    ORG(I)                                 28
031000000706IF  2C     PARPOS        IFEQ      'N'                                          NO DATI POSTE
031100020627     C     POS(I)        ANDEQ     'PPT'                                        *TFP POSTE
031200000706     C     PARPOS        OREQ      'S'                                          SI DATI POSTE
031300020627     C     POS(I)        ANDNE     'PPT'                                        *TFP NO POSTE
031400000706     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031500000706E   2C                   ENDIF
031600000706E   1C                   ENDIF
031700000706     C*
031800000000     C                   ENDSR
031900000000     C*------------------------------------------------------------------------*
032000000000     C* MEMSAL - MEMORIZZA I DATI
032100000000     C*------------------------------------------------------------------------*
032200000000     C     MEMSAL        BEGSR
032300000000     C* MESE
032400000000     C                   Z-ADD     1             M                 2 0
032500000000     C                   SETOFF                                           28
032600000000     C     DEPMES        LOOKUP    MES(M)                                 28
032700960927IF  1C     *IN28         IFEQ      *ON
032800000000     C* DECADE
032900000000     C                   MOVEL     DEPDEC        D                 1 0
033000000000     C***
033100000000     C* FILIALE
033200000000     C***
033300000000     C     D             OCCUR     TDEC
033400000000     C                   MOVEL     '1'           PASTFP                         *MEMORIZZATO QC TF1
033500960927     C                   ADD       WFLPAR        TPAR(M)                        *TOTALE PARTENZE
033600960927     C                   ADD       WFLRIT        TRIT(M)                        *TOTALE RITARDI
033700960927     C                   ADD       WFLFIL        TFIL(M)                        *COLPA FILIALE
033800960927     C                   ADD       WFLAUT        TAUT(M)                        *COLPA AUTISTA
033900960927     C                   ADD       WFLCFM        TCFM(M)                        *CAUSA FORZA MAGGIOR
034000960927     C                   ADD       WFLSED        TSED(M)                        *TRINI SEDE
034100960927     C                   ADD       WFLTRS        TTRS(M)                        *TRANSITO
034200990817     C                   ADD       WFLFIO        TFIO(M)                        *FILIALE ARR ORARIO
034300000000     C***
034400000000     C* DIVISIONE
034500000000     C***
034600000000     C     D             OCCUR     DDEC
034700000000     C                   MOVEL     '1'           PASDIV                         *MEMORIZZATO QC DIV
034800960927     C                   ADD       WFLPAR        DPAR(M)                        *TOTALE PARTENZE
034900960927     C                   ADD       WFLRIT        DRIT(M)                        *TOTALE RITARDI
035000960927     C                   ADD       WFLFIL        DFIL(M)                        *COLPA FILIALE
035100960927     C                   ADD       WFLAUT        DAUT(M)                        *COLPA AUTISTA
035200960927     C                   ADD       WFLCFM        DCFM(M)                        *CAUSA FORZA MAGGIOR
035300960927     C                   ADD       WFLSED        DSED(M)                        *TRINI SEDE
035400960927     C                   ADD       WFLTRS        DTRS(M)                        *TRANSITO
035500990817     C                   ADD       WFLFIO        DFIO(M)                        *FILIALE ARR ORARIO
035600000000     C***
035700000000     C* GENERALE
035800000000     C***
035900000000     C     D             OCCUR     GDEC
036000000000     C                   MOVEL     '1'           PASGEN                         *MEMORIZZATO QC GEN
036100960927     C                   ADD       WFLPAR        GPAR(M)                        *TOTALE PARTENZE
036200960927     C                   ADD       WFLRIT        GRIT(M)                        *TOTALE RITARDI
036300960927     C                   ADD       WFLFIL        GFIL(M)                        *COLPA FILIALE
036400960927     C                   ADD       WFLAUT        GAUT(M)                        *COLPA AUTISTA
036500960927     C                   ADD       WFLCFM        GCFM(M)                        *CAUSA FORZA MAGGIOR
036600960927     C                   ADD       WFLSED        GSED(M)                        *TRINI SEDE
036700960927     C                   ADD       WFLTRS        GTRS(M)                        *TRANSITO
036800990817     C                   ADD       WFLFIO        GFIO(M)                        *FILIALE ARR ORARIO
036900000000E   1C                   ENDIF
037000000000     C*
037100000000     C                   ENDSR
037200000000     C*------------------------------------------------------------------------*
037300000000     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
037400000000     C*------------------------------------------------------------------------*
037500000000     C     NEWDIV        BEGSR
037600000000     C*
037700000000     C* REIMPOSTA CONTATORE BLOCCHI STAMPATI
037800000000     C                   Z-ADD     *ZEROS        CONBLO                         *AZZERA IL CONTATAOR
037900000000     C*
038000000000     C* REIMPOSTA IL FLAG DI PASSAGGIO MEMORIZZAZIONE DIVISIONE
038100000000     C                   MOVEL     '0'           PASDIV
038200000000     C*
038300000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
038400960927     C                   MOVEL     WFLDIV        DEPDIV
038500000000     C*
038600000000     C* DECODIFICA LA DIVISIONE
038700960927     C                   SETOFF                                           28
038800960927     C                   Z-ADD     1             I
038900960927     C     DEPDIV        LOOKUP    DIV(I)                                 28
039000960927IF  1C     *IN28         IFEQ      *ON
039100960927     C                   MOVEL(P)  DDIV(I)       DECDIV           20
039200960927X   1C                   ELSE
039300960927     C                   MOVEL     *ALL'*'       DECDIV           20
039400960927E   1C                   ENDIF
039500000000     C*
039600000000     C* PULISCE I TOTALI PER DIVISIONE
039700000000DO  1C     1             DO        3             I
039800000000     C     I             OCCUR     DDEC
039900000000     C                   CLEAR                   DPAR
040000000000     C                   CLEAR                   DRIT
040100000000     C                   CLEAR                   DFIL
040200000000     C                   CLEAR                   DAUT
040300000000     C                   CLEAR                   DCFM
040400000000     C                   CLEAR                   DSED
040500000000     C                   CLEAR                   DTRS
040600990817     C                   CLEAR                   DFIO
040700000000E   1C                   ENDDO
040800000000     C*
040900000000     C                   ENDSR
041000000000     C*------------------------------------------------------------------------*
041100000000     C* NEWTFP - OPERAZIONI PER NUOVA FILIALE
041200000000     C*------------------------------------------------------------------------*
041300000000     C     NEWTFP        BEGSR
041400000000     C*
041500000000     C* REIMPOSTA IL FLAG DI PASSAGGIO MEMORIZZAZIONE FILIALE
041600000000     C                   MOVEL     '0'           PASTFP
041700000000     C*
041800000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
041900960927     C                   MOVEL     WFLTFP        DEPTFP
042000000000     C*
042100000000     C* DECODIFICA LA FILIALE
042200960927     C                   SETOFF                                           28
042300960927     C                   Z-ADD     1             I
042400960927     C     DEPTFP        LOOKUP    ORG(I)                                 28
042500960927IF  1C     *IN28         IFEQ      *ON
042600960927     C                   MOVEL(P)  DORG(I)       DECTFP           20
042700960927X   1C                   ELSE
042800960927     C                   MOVEL     *ALL'*'       DECTFP           20
042900960927E   1C                   ENDIF
043000000000     C*
043100000000     C* PULISCE I TOTALI PER FILIALE
043200000000DO  1C     1             DO        3             I
043300000000     C     I             OCCUR     TDEC
043400000000     C                   CLEAR                   TPAR
043500000000     C                   CLEAR                   TRIT
043600000000     C                   CLEAR                   TFIL
043700000000     C                   CLEAR                   TAUT
043800000000     C                   CLEAR                   TCFM
043900000000     C                   CLEAR                   TSED
044000000000     C                   CLEAR                   TTRS
044100990817     C                   CLEAR                   TFIO
044200000000E   1C                   ENDDO
044300000000     C*
044400000000     C                   ENDSR
044500000000     C*------------------------------------------------------------------------*
044600000000     C* NEWMES - OPERAZIONI PER NUOVO MESE
044700000000     C*------------------------------------------------------------------------*
044800000000     C     NEWMES        BEGSR
044900000000     C*
045000960927     C* MEMORIZZA IL NUOVO MESE IN UN DEPOSITO
045100960927     C                   Z-ADD     WFLMES        DEPMES
045200000000     C*
045300000000     C                   ENDSR
045400000000     C*------------------------------------------------------------------------*
045500000000     C* NEWDEC - OPERAZIONI PER NUOVA DECADE
045600000000     C*------------------------------------------------------------------------*
045700000000     C     NEWDEC        BEGSR
045800960927     C*
045900960927     C* MEMORIZZA LA NUOVA DECADE IN UN DEPOSITO
046000960927     C                   MOVEL     WFLDEC        DEPDEC
046100000000     C*
046200000000     C                   ENDSR
046300000000     C*------------------------------------------------------------------------*
046400000000     C* STATFP - STAMPA TOTALI PER FILIALE PARTENZA
046500000000     C*------------------------------------------------------------------------*
046600000000     C     STATFP        BEGSR
046700000000     C*
046800000000     C* STAMPA LA PAGINA SOLO SE HA MEMORIZZATO QC
046900000000IF  1C     PASTFP        IFEQ      '1'
047000000000     C*
047100000000     C* IMPOSTA L'INTESTAZIONE
047200000000     C                   MOVEL     DRI(16)       PDCDES                         *DESCR "FILIALE"
047300000000     C                   MOVEL(P)  DECTFP        PDDTFP                         *DESCR FILIALE
047400000000     C*
047500000000     C* SCHIERE DI MEMORIZZAZIONE --> SCHIERE DI STAMPA
047600000000DO  2C     1             DO        3             I
047700000000     C     I             OCCUR     TDEC
047800000000     C     I             OCCUR     SDEC
047900000000     C                   Z-ADD     TPAR          SPAR
048000000000     C                   Z-ADD     TRIT          SRIT
048100000000     C                   Z-ADD     TFIL          SFIL
048200000000     C                   Z-ADD     TAUT          SAUT
048300000000     C                   Z-ADD     TCFM          SCFM
048400000000     C                   Z-ADD     TSED          SSED
048500000000     C                   Z-ADD     TTRS          STRS
048600990817     C                   Z-ADD     TFIO          SFIO
048700000000E   2C                   ENDDO
048800960903     C*---
048900960903     C* STAMPA I TOTALI FILIALE DI SEGUITO SE RICHIESTO IL TOTALE PER DIVISIONE
049000960903     C* STAMPA I TOTALI FILIALE IN UNA PAG NUOVA SE RICHIESTO IL TOTALE PER AREA
049100960903     C*---
049200960903     C* CONTEGGIA QUESTO NEL NUMERO DEI BLOCCHI STAMPATI (MAX 3 PER PAG)
049300960903IF  2C     PARTAD        IFEQ      'D'
049400960903     C                   ADD       1             CONBLO                         *DI SEGUITO
049500960903     C*
049600960903     C* CONTEGGIA QUESTO NEL NUMERO DEI BLOCCHI STAMPATI (MAX 3 PER PAG)
049700960903X   2C                   ELSE
049800960903IF  3C     $TOUR         IFEQ      '0'                                          *SE PRIMO GIRO
049900960903     C                   MOVEL     '1'           $TOUR
050000960903     C                   MOVEL     '1'           *IN01                          *NO STAMPA RIGA CHIU
050100960903X   3C                   ELSE                                                   *SE NO PRIMO GIRO
050200960903     C                   Z-ADD     4             CONBLO                         *RIGA CHIUS + NEW PA
050300960903E   3C                   ENDIF
050400960903E   2C                   ENDIF
050500000000     C* STAMPA TOTALI
050600000000     C                   EXSR      STAMPA
050700000000E   1C                   ENDIF
050800000000     C*
050900000000     C                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* STADIV - STAMPA TOTALI PER DIVISIONE
051200000000     C*------------------------------------------------------------------------*
051300000000     C     STADIV        BEGSR
051400000000     C*
051500000000     C* STAMPA LA PAGINA SOLO SE HA MEMORIZZATO QC
051600000000IF  1C     PASDIV        IFEQ      '1'
051700000000     C*
051800000000     C* IMPOSTA L'INTESTAZIONE
051900000000     C                   MOVEL     DRI(17)       PDCDES                         *DESCR "DIVISIONE"
052000000000     C                   MOVEL(P)  DECDIV        PDDTFP                         *DESCR DIVISIONE
052100000000     C*
052200000000     C* SCHIERE DI MEMORIZZAZIONE --> SCHIERE DI STAMPA
052300000000DO  2C     1             DO        3             I
052400000000     C     I             OCCUR     DDEC
052500000000     C     I             OCCUR     SDEC
052600000000     C                   Z-ADD     DPAR          SPAR
052700000000     C                   Z-ADD     DRIT          SRIT
052800000000     C                   Z-ADD     DFIL          SFIL
052900000000     C                   Z-ADD     DAUT          SAUT
053000000000     C                   Z-ADD     DCFM          SCFM
053100000000     C                   Z-ADD     DSED          SSED
053200000000     C                   Z-ADD     DTRS          STRS
053300990817     C                   Z-ADD     DFIO          SFIO
053400000000E   2C                   ENDDO
053500000000     C*
053600000000     C* CONTEGGIA QUESTO NEL NUMERO DEI BLOCCHI STAMPATI (MAX 3 PER PAG)
053700000000     C                   Z-ADD     4             CONBLO                         *PAGINA NUOVA
053800000000     C* STAMPA TOTALI
053900000000     C                   EXSR      STAMPA
054000960903     C                   WRITE     SS88RI                                       *STAMPA RIGA CHIUSUR
054100000000     C                   SETON                                        01        *PAGINA SUCCESSIVA
054200000000E   1C                   ENDIF
054300000000     C*
054400000000     C                   ENDSR
054500000000     C*------------------------------------------------------------------------*
054600000000     C* STAGEN - STAMPA TOTALI GENERALI
054700000000     C*------------------------------------------------------------------------*
054800000000     C     STAGEN        BEGSR
054900000000     C*
055000000000     C* STAMPA LA PAGINA SOLO SE HA MEMORIZZATO QC
055100000000IF  1C     PASGEN        IFEQ      '1'
055200000000     C*
055300000000     C* IMPOSTA L'INTESTAZIONE
055400000000     C                   MOVEL     DRI(18)       PDCDES                         *DESCR "TOTALE"
055500000000     C                   MOVEL(P)  DRI(19)       PDDTFP                         *DESCR "GENERALE"
055600000000     C*
055700000000     C* SCHIERE DI MEMORIZZAZIONE --> SCHIERE DI STAMPA
055800000000DO  2C     1             DO        3             I
055900000000     C     I             OCCUR     GDEC
056000000000     C     I             OCCUR     SDEC
056100000000     C                   Z-ADD     GPAR          SPAR
056200000000     C                   Z-ADD     GRIT          SRIT
056300000000     C                   Z-ADD     GFIL          SFIL
056400000000     C                   Z-ADD     GAUT          SAUT
056500000000     C                   Z-ADD     GCFM          SCFM
056600000000     C                   Z-ADD     GSED          SSED
056700000000     C                   Z-ADD     GTRS          STRS
056800990817     C                   Z-ADD     GFIO          SFIO
056900000000E   2C                   ENDDO
057000000000     C*
057100000000     C* CONTEGGIA QUESTO NEL NUMERO DEI BLOCCHI STAMPATI (MAX 3 PER PAG)
057200000000     C                   Z-ADD     4             CONBLO                         *PAGINA NUOVA
057300000000     C* STAMPA TOTALI
057400000000     C                   EXSR      STAMPA
057500000000     C                   WRITE     SS88RI                                       *STAMPA RIGA CHIUSUR
057600960903E   1C                   ENDIF
057700000000     C*
057800000000     C                   ENDSR
057900000000     C*------------------------------------------------------------------------*
058000000000     C* STAMPA - STAMPA TOTALI
058100000000     C*------------------------------------------------------------------------*
058200000000     C     STAMPA        BEGSR
058300000000     C*
058400000000     C* PULISCE LE SCHIERE DI LAVORO
058500000000     C                   CLEAR                   $PAR
058600000000     C                   CLEAR                   $RIT
058700000000     C                   CLEAR                   $FIL
058800000000     C                   CLEAR                   $AUT
058900000000     C                   CLEAR                   $CFM
059000000000     C                   CLEAR                   $SED
059100000000     C                   CLEAR                   $TRS
059200990817     C                   CLEAR                   $FIO
059300000000     C*
059400000000     C* PULISCE LE SCHIERE DI PASSAGGIO
059500000000     C                   CLEAR                   PV1
059600000000     C                   CLEAR                   PP1
059700000000     C*--------------------
059800000000     C* RAGGRUPPA I DATI PER MESE
059900000000     C*--------------------
060000000000     C* PER CIASCUN MESE MEMORIZZATO SOMMA I DATI DELLE TRE DECADI
060100000000DO  1C     1             DO        DEPM          M
060200000000DO  2C     1             DO        3             D
060300000000     C     D             OCCUR     SDEC
060400000000     C                   ADD       SPAR(M)       $PAR(M)
060500000000     C                   ADD       SRIT(M)       $RIT(M)
060600000000     C                   ADD       SFIL(M)       $FIL(M)
060700000000     C                   ADD       SAUT(M)       $AUT(M)
060800000000     C                   ADD       SCFM(M)       $CFM(M)
060900000000     C                   ADD       SSED(M)       $SED(M)
061000000000     C                   ADD       STRS(M)       $TRS(M)
061100990817     C                   ADD       SFIO(M)       $FIO(M)
061200000000     C*
061300000000     C* TIENE L'ULTIMO MESE CON LE DECADI SEPARATE
061400000000IF  3C     M             IFEQ      DEPM
061500000000     C                   Z-ADD     *ZEROS        I
061600000000     C     12            ADD       D             I
061700000000     C                   ADD       SPAR(M)       $PAR(I)
061800000000     C                   ADD       SRIT(M)       $RIT(I)
061900000000     C                   ADD       SFIL(M)       $FIL(I)
062000000000     C                   ADD       SAUT(M)       $AUT(I)
062100000000     C                   ADD       SCFM(M)       $CFM(I)
062200000000     C                   ADD       SSED(M)       $SED(I)
062300000000     C                   ADD       STRS(M)       $TRS(I)
062400990817     C                   ADD       SFIO(M)       $FIO(I)
062500000000E   3C                   ENDIF
062600000000E   2C                   ENDDO                                                  *FINE DECADI
062700000000E   1C                   ENDDO                                                  *FINE MESI
062800000000     C* TOTALE MESI
062900000000     C                   Z-ADD     16            I                              *ALLA 16^ COLONNA
063000000000DO  1C     1             DO        DEPM          M
063100000000     C                   ADD       $PAR(M)       $PAR(I)
063200000000     C                   ADD       $RIT(M)       $RIT(I)
063300000000     C                   ADD       $FIL(M)       $FIL(I)
063400000000     C                   ADD       $AUT(M)       $AUT(I)
063500000000     C                   ADD       $CFM(M)       $CFM(I)
063600000000     C                   ADD       $SED(M)       $SED(I)
063700000000     C                   ADD       $TRS(M)       $TRS(I)
063800990817     C                   ADD       $FIO(M)       $FIO(I)
063900000000E   1C                   ENDDO
064000000000     C*--------------------
064100000000     C* STAMPA TESTATA
064200000000     C*--------------------
064300000000IF  1C     CONBLO        IFGT      3                                            *MAX 3 BLOCCHI X PAG
064400000000     C     *IN01         OREQ      *ON
064500960903IF  2C     CONBLO        IFGT      3                                            *MAX 3 BLOCCHI X PAG
064600000000     C                   WRITE     SS88RI                                       *STAMPA RIGA CHIUSUR
064700960903E   2C                   ENDIF
064800000000     C                   WRITE     SS88PT                                       *TESTATA
064900000000IF  2C     CONBLO        IFGT      3
065000000000     C                   Z-ADD     1             CONBLO                         *AZZERA IL CONTATAOR
065100000000E   2C                   ENDIF
065200000000     C                   SETOFF                                       01        *AZZERA OVERFLOW
065300000000E   1C                   ENDIF
065400000000     C*--------------------
065500000000     C* STAMPA RIGA
065600000000     C*--------------------
065700000000     C***
065800000000     C* INTESTAZIONE
065900000000     C***
066000000000     C                   WRITE     SS88PI                                       *INTESTAZIONE
066100000000     C***
066200000000     C* TOTALE PARTENZE
066300000000     C***
066400000000     C                   Z-ADD     $PAR          PV1
066500000000     C                   MOVEL     DRI(1)        WDES             30
066600000000     C                   EXSR      STAVAL
066700000000     C***
066800000000     C* TOTALE RITARDI
066900000000     C***
067000000000     C                   Z-ADD     $RIT          PV1
067100000000     C                   MOVEL     DRI(2)        WDES             30
067200000000     C                   EXSR      STAVAL
067300000000     C*
067400000000     C* 100 : X = TOTALE PARTENZE : TOTALE RITARDI
067500000000     C* 100 * TOTALE RITARDI / TOTALE PARTENZE
067600000000     C                   CLEAR                   PP1
067700000000DO  1C     1             DO        16            M
067800000000     C                   Z-ADD     $RIT(M)       WDIVID           18 3
067900000000     C                   Z-ADD     $PAR(M)       WDIVIS           18 3
068000000000     C                   Z-ADD     *ZEROS        WPERC             6 3
068100000000     C                   EXSR      CALPE2
068200000000     C                   Z-ADD     WPERC         PP1(M)
068300000000E   1C                   ENDDO
068400990817     C                   MOVEL     DRI(10)       WDES
068500000000     C                   EXSR      STAPER
068600000000     C***
068700000000     C* DI CUI:
068800000000     C***
068900000000     C                   Z-ADD     *ZEROS        PV1
069000000000     C                   MOVEL     DRI(3)        WDES             30
069100000000     C                   EXSR      STAVAL
069200000000     C***
069300990817     C* FILIALE / ARRIVO IN RITARDO
069400000000     C***
069500000000     C                   Z-ADD     $FIL          PV1
069600000000     C                   MOVEL     DRI(4)        WDES             30
069700000000     C                   EXSR      STAVAL
069800000000     C*
069900000000     C* 100 : X = TOTALE PARTENZE : FILIALE
070000000000     C* 100 * FILIALE / TOTALE PARTENZE
070100000000     C                   CLEAR                   PP1
070200000000DO  1C     1             DO        16            M
070300000000     C                   Z-ADD     $FIL(M)       WDIVID           18 3
070400000000     C                   Z-ADD     $PAR(M)       WDIVIS           18 3
070500000000     C                   Z-ADD     *ZEROS        WPERC             6 3
070600000000     C                   EXSR      CALPE2
070700000000     C                   Z-ADD     WPERC         PP1(M)
070800000000E   1C                   ENDDO
070900990817     C                   MOVEL     DRI(10)       WDES
071000000000     C                   EXSR      STAPER
071100000000     C***
071200000000     C* AUTISTA
071300000000     C***
071400000000     C                   Z-ADD     $AUT          PV1
071500000000     C                   MOVEL     DRI(5)        WDES             30
071600000000     C                   EXSR      STAVAL
071700000000     C*
071800000000     C* 100 : X = TOTALE PARTENZE : AUTISTA
071900000000     C* 100 * AUTISTA / TOTALE PARTENZE
072000000000     C                   CLEAR                   PP1
072100000000DO  1C     1             DO        16            M
072200000000     C                   Z-ADD     $AUT(M)       WDIVID           18 3
072300000000     C                   Z-ADD     $PAR(M)       WDIVIS           18 3
072400000000     C                   Z-ADD     *ZEROS        WPERC             6 3
072500000000     C                   EXSR      CALPE2
072600000000     C                   Z-ADD     WPERC         PP1(M)
072700000000E   1C                   ENDDO
072800990817     C                   MOVEL     DRI(10)       WDES
072900000000     C                   EXSR      STAPER
073000000000     C***
073100000000     C* CAUSA FORZA MAGGIORE
073200000000     C***
073300000000     C                   Z-ADD     $CFM          PV1
073400000000     C                   MOVEL     DRI(6)        WDES             30
073500000000     C                   EXSR      STAVAL
073600000000     C*
073700000000     C* 100 : X = TOTALE PARTENZE : CAUSA FORZA MAGGIORE
073800000000     C* 100 * CAUSA FORZA MAGGIORE / TOTALE PARTENZE
073900000000     C                   CLEAR                   PP1
074000000000DO  1C     1             DO        16            M
074100000000     C                   Z-ADD     $CFM(M)       WDIVID           18 3
074200000000     C                   Z-ADD     $PAR(M)       WDIVIS           18 3
074300000000     C                   Z-ADD     *ZEROS        WPERC             6 3
074400000000     C                   EXSR      CALPE2
074500000000     C                   Z-ADD     WPERC         PP1(M)
074600000000E   1C                   ENDDO
074700990817     C                   MOVEL     DRI(10)       WDES
074800000000     C                   EXSR      STAPER
074900000000     C***
075000000000     C* SEDE
075100000000     C***
075200000000     C                   Z-ADD     $SED          PV1
075300000000     C                   MOVEL     DRI(7)        WDES             30
075400000000     C                   EXSR      STAVAL
075500000000     C*
075600000000     C* 100 : X = TOTALE PARTENZE : SEDE
075700000000     C* 100 * SEDE / TOTALE PARTENZE
075800000000     C                   CLEAR                   PP1
075900000000DO  1C     1             DO        16            M
076000000000     C                   Z-ADD     $SED(M)       WDIVID           18 3
076100000000     C                   Z-ADD     $PAR(M)       WDIVIS           18 3
076200000000     C                   Z-ADD     *ZEROS        WPERC             6 3
076300000000     C                   EXSR      CALPE2
076400000000     C                   Z-ADD     WPERC         PP1(M)
076500000000E   1C                   ENDDO
076600990817     C                   MOVEL     DRI(10)       WDES
076700000000     C                   EXSR      STAPER
076800000000     C***
076900000000     C* TRANSITO
077000000000     C***
077100000000     C                   Z-ADD     $TRS          PV1
077200000000     C                   MOVEL     DRI(8)        WDES             30
077300000000     C                   EXSR      STAVAL
077400000000     C*
077500000000     C* 100 : X = TOTALE PARTENZE : TRANSITO
077600000000     C* 100 * TRANSITO / TOTALE PARTENZE
077700000000     C                   CLEAR                   PP1
077800000000DO  1C     1             DO        16            M
077900000000     C                   Z-ADD     $TRS(M)       WDIVID           18 3
078000000000     C                   Z-ADD     $PAR(M)       WDIVIS           18 3
078100000000     C                   Z-ADD     *ZEROS        WPERC             6 3
078200000000     C                   EXSR      CALPE2
078300000000     C                   Z-ADD     WPERC         PP1(M)
078400000000E   1C                   ENDDO
078500990817     C                   MOVEL     DRI(10)       WDES
078600000000     C                   EXSR      STAPER
078700990817     C***
078800990817     C* FILIALE / ARRIVO IN ORARIO
078900990817     C***
079000990817     C                   Z-ADD     $FIO          PV1
079100990817     C                   MOVEL     DRI(9)        WDES             30
079200990817     C                   EXSR      STAVAL
079300990817     C*
079400990817     C* 100 : X = TOTALE PARTENZE : FILIALE ARRIVO IN ORARIO
079500990817     C* 100 * FILIALE ARRIVO IN ORARIO / TOTALE PARTENZE
079600990817     C                   CLEAR                   PP1
079700990817DO  1C     1             DO        16            M
079800990817     C                   Z-ADD     $FIO(M)       WDIVID           18 3
079900990817     C                   Z-ADD     $PAR(M)       WDIVIS           18 3
080000990817     C                   Z-ADD     *ZEROS        WPERC             6 3
080100990817     C                   EXSR      CALPE2
080200990817     C                   Z-ADD     WPERC         PP1(M)
080300990817E   1C                   ENDDO
080400990817     C                   MOVEL     DRI(10)       WDES
080500990817     C                   EXSR      STAPER
080600990817     C*
080700000000     C                   ENDSR
080800000000     C*------------------------------------------------------------------------*
080900000000     C* STAVAL - STAMPA UNA RIGA DI VALORE
081000000000     C*------------------------------------------------------------------------*
081100000000     C     STAVAL        BEGSR
081200000000     C*
081300000000     C                   CLEAR                   SS88P1                         *PULISCE RIGA
081400000000     C                   MOVEL     WDES          P1DDES                         *DESCRIZIONE VALORE
081500000000     C                   Z-ADD     PV1(1)        P1CVA1                         *MESE UNO
081600000000     C                   Z-ADD     PV1(2)        P1CVA2                         *MESE DUE
081700000000     C                   Z-ADD     PV1(3)        P1CVA3                         *MESE TRE
081800000000     C                   Z-ADD     PV1(4)        P1CVA4                         *MESE QUATTRO
081900000000     C                   Z-ADD     PV1(5)        P1CVA5                         *MESE CINQUE
082000000000     C                   Z-ADD     PV1(6)        P1CVA6                         *MESE SEI
082100000000     C                   Z-ADD     PV1(7)        P1CVA7                         *MESE SETTE
082200000000     C                   Z-ADD     PV1(8)        P1CVA8                         *MESE OTTO
082300000000     C                   Z-ADD     PV1(9)        P1CVA9                         *MESE NOVE
082400000000     C                   Z-ADD     PV1(10)       P1CVA0                         *MESE DIECI
082500000000     C                   Z-ADD     PV1(11)       P1CVAA                         *MESE UNDICI
082600000000     C                   Z-ADD     PV1(12)       P1CVAB                         *MESE DODICI
082700000000     C                   Z-ADD     PV1(13)       P1CDE1                         *1^DECADE
082800000000     C                   Z-ADD     PV1(14)       P1CDE2                         *2^DECADE
082900000000     C                   Z-ADD     PV1(15)       P1CDE3                         *3^DECADE
083000000000     C                   Z-ADD     PV1(16)       P1CVAT                         *TOTALE 12 MESI
083100000000     C*
083200000000     C                   WRITE     SS88P1                                       *STAMPA RIGA
083300000000     C*
083400000000     C                   ENDSR
083500000000     C*------------------------------------------------------------------------*
083600000000     C* STAPER - STAMPA UNA RIGA DI PERCENTUALE
083700000000     C*------------------------------------------------------------------------*
083800000000     C     STAPER        BEGSR
083900000000     C*
084000000000     C                   CLEAR                   SS88P2                         *PULISCE RIGA
084100000000     C                   MOVEL     WDES          P2DDES                         *DESCRIZ.PERCENTUALE
084200000000     C                   Z-ADD     PP1(1)        P2CVA1                         *MESE UNO
084300000000     C                   Z-ADD     PP1(2)        P2CVA2                         *MESE DUE
084400000000     C                   Z-ADD     PP1(3)        P2CVA3                         *MESE TRE
084500000000     C                   Z-ADD     PP1(4)        P2CVA4                         *MESE QUATTRO
084600000000     C                   Z-ADD     PP1(5)        P2CVA5                         *MESE CINQUE
084700000000     C                   Z-ADD     PP1(6)        P2CVA6                         *MESE SEI
084800000000     C                   Z-ADD     PP1(7)        P2CVA7                         *MESE SETTE
084900000000     C                   Z-ADD     PP1(8)        P2CVA8                         *MESE OTTO
085000000000     C                   Z-ADD     PP1(9)        P2CVA9                         *MESE NOVE
085100000000     C                   Z-ADD     PP1(10)       P2CVA0                         *MESE DIECI
085200000000     C                   Z-ADD     PP1(11)       P2CVAA                         *MESE UNDICI
085300000000     C                   Z-ADD     PP1(12)       P2CVAB                         *MESE DODICI
085400000000     C                   Z-ADD     PP1(13)       P2CDE1                         *1^DECADE
085500000000     C                   Z-ADD     PP1(14)       P2CDE2                         *2^DECADE
085600000000     C                   Z-ADD     PP1(15)       P2CDE3                         *3^DECADE
085700000000     C                   Z-ADD     PP1(16)       P2CVAT                         *TOTALE 12 MESI
085800000000     C*
085900000000     C                   WRITE     SS88P2                                       *STAMPA RIGA
086000000000     C*
086100000000     C                   ENDSR
086200000000     C*------------------------------------------------------------------------*
086300000000     C* CALPE2 - CALCOLA LA PERCENTUALE
086400000000     C*          INPUT  -> WDIVID = DIVIDENDO / WDIVIS = DIVISORE
086500000000     C*          OUTPUT -> WPERC  = PERCENTUALE
086600000000     C*------------------------------------------------------------------------*
086700000000     C     CALPE2        BEGSR
086800000000     C*
086900000000     C*   DIVIDENDO *  100
087000000000     C*  ------------------ =  PERCENTUALE
087100000000     C*        DIVISORE
087200000000     C***
087300000000     C* A) IL DIVIDENDO CHE IL DIVISORE SONO = ZERO;
087400000000     C*    PERCENTUALE = 0
087500000000     C* B) IL DIVIDENDO E' = ZERO MENTRE IL DIVISORE NO;
087600000000     C*    PERCENTUALE = 0
087700000000     C* C) IL DIVISORE E' = ZERO MENTRE IL DIVIDENDO NO;
087800000000     C*    PERCENTUALE = 0
087900000000     C* D) IL DIVIDENDO E IL DIVISORE SONO > ZERO;
088000000000     C*    PERCENTUALE = +/-XXX,X
088100000000     C***
088200000000IF  1C     WDIVID        IFEQ      *ZEROS
088300000000IF  2C     WDIVIS        IFEQ      *ZEROS
088400000000     C                   Z-ADD     *ZEROS        WPERC                          *CASO A
088500000000X   2C                   ELSE
088600000000     C                   Z-ADD     *ZEROS        WPERC                          *CASO B
088700000000E   2C                   ENDIF
088800000000X   1C                   ELSE
088900000000IF  2C     WDIVIS        IFEQ      *ZEROS
089000000000     C                   Z-ADD     *ZEROS        WPERC                          *CASO C
089100000000X   2C                   ELSE
089200000000     C     WDIVID        MULT      100           WNUM19           19 3          *CASO D
089300000000     C     WNUM19        DIV       WDIVIS        WNUM19
089400000000     C*
089500000000     C* SE PERCENTUALE = +/-1000 METTE +/-999,9
089600000000     C                   SETOFF                                           28
089700000000     C                   Z-ADD     WNUM19        WPERC
089800000000     C     WNUM19        COMP      0                                    28
089900000000IF  3C     *IN28         IFEQ      *OFF                                         *POSITIVO
090000000000IF  4C     WNUM19        IFGE      1000
090100000000     C                   Z-ADD     +999,9        WPERC
090200000000E   4C                   ENDIF
090300000000X   3C                   ELSE                                                   *NEGATIVO
090400000000IF  4C     WNUM19        IFLE      -1000
090500000000     C                   Z-ADD     -999,9        WPERC
090600000000E   4C                   ENDIF
090700000000E   3C                   ENDIF
090800000000E   2C                   ENDIF
090900000000E   1C                   ENDIF
091000000000     C*
091100000000     C                   ENDSR
091200000000     C*------------------------------------------------------------------------*
091300000000     C* STAPAR - STAMPA LA PAGINA DEI PARAMETRI
091400000000     C*------------------------------------------------------------------------*
091500000000     C     STAPAR        BEGSR
091600960903     C*---
091700960903     C* TASTATA
091800960903     C*---
091900960903IF  1C     PARTAD        IFEQ      'D'
092000960903     C                   MOVEL     DRI(14)       PTDTAD
092100960903X   1C                   ELSE
092200960903     C                   MOVEL     DRI(15)       PTDTAD
092300960903E   1C                   ENDIF
092400960903     C*---
092500960903     C* PARAMETRI
092600960903     C*---
092700000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
092800000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
092900000706IF  1C     PARCRE        IFEQ      'S'
093000000706     C                   MOVEL     'SI'          PADCRE
093100000706X   1C                   ELSE
093200020220     C                   MOVEL     'NO'          PADCRE
093300000706E   1C                   ENDIF
093400000706IF  1C     PARPOS        IFEQ      'S'
093500000706     C                   MOVEL     'SI'          PADPOS
093600000706X   1C                   ELSE
093700000710     C                   MOVEL     'NO'          PADPOS
093800000706E   1C                   ENDIF
093900960903     C* STAMPA
094000000000     C                   WRITE     SS88PT                                       *INTESTAZIONE
094100000000     C                   WRITE     SS88PA                                       *PARAMETRI
094200000000     C*
094300000000     C                   ENDSR
094400000000     C*------------------------------------------------------------------------*
094500000000     C* CARTAB - CARICA LE TABELLE
094600000000     C*------------------------------------------------------------------------*
094700000000     C     CARTAB        BEGSR
094800000000     C*--------------------
094900000000     C* FILIALI
095000000000     C*--------------------
095100000000     C                   Z-ADD     *ZEROS        I                 4 0
095200960927     C                   READ      AZORG01L                               99
095300000000DO  1C     *IN99         DOWEQ     *OFF
095400000000IF  2C     ORGFVA        IFEQ      *BLANKS
095500000000     C     ORGFAG        ANDNE     'V'
095600000706     C                   MOVEL     ORGDE3        OG143
095700000000     C                   ADD       1             I
095800960927     C                   Z-ADD     ORGFIL        ORG(I)                         *FILIALE
095900000000     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
096000020627     C                   MOVEL     §OGNTW        POS(I)                         *FLAG POSTE
096100000706     C                   MOVEL     ORGFL3        DIV(I)                         *DIVISIONE
096200020906     C                   Z-ADD     1             KTBKUT
096300000000     C                   MOVEL     '17'          KTBCOD
096400000000     C                   MOVEL(P)  DIV(I)        KTBKEY
096500000000     C     KEYTAB        CHAIN     TABEL00F                           98
096600000000IF  3C     *IN98         IFEQ      *OFF
096700000000     C                   MOVEL     TBLUNI        DDIV(I)
096800000000E   3C                   ENDIF
096900000000E   2C                   ENDIF
097000960927     C                   READ      AZORG01L                               99
097100000000E   1C                   ENDDO
097200000000     C*
097300000000     C                   ENDSR
097400000000     C*------------------------------------------------------------------------*
097500000000     C* CARMES - CARICA I MESI DA ELEBORARE
097600000000     C*------------------------------------------------------------------------*
097700000000     C     CARMES        BEGSR
097800000000     C*
097900000000     C                   CLEAR                   ANN                            *ANNI
098000000000     C                   CLEAR                   MES                            *MESI
098100000000     C                   Z-ADD     *ZEROS        J                 4 0          *N° MESI DA ELABORAR
098200990608     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
098300000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
098400990608     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
098500000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
098600000000     C                   Z-ADD     *ZEROS        DEPM              4 0          *MESI SELEZIONATI
098700000000     C***
098800000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
098900000000     C***
099000000000     C* CALCOLA I MESI DI DIFFERENZA E LASCIA IL RESTO DELLA SCHIERA A ZERO
099100000000IF  1C     WAA2          IFEQ      WAA
099200000000DO  2C     WMM           DOWLE     WMM2
099300000000     C                   ADD       1             J
099400000000     C                   Z-ADD     WAA           ANN(J)
099500000000     C                   Z-ADD     WMM           MES(J)
099600000000     C                   ADD       1             WMM
099700000000E   2C                   ENDDO
099800000000     C                   Z-ADD     J             DEPM                           *MESI SELEZIONATI
099900000000DO  2C     J             DOWLT     12
100000000000     C                   ADD       1             J
100100000000     C                   Z-ADD     *ZEROS        ANN(J)
100200000000     C                   Z-ADD     *ZEROS        MES(J)
100300000000E   2C                   ENDDO
100400000000     C***
100500000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
100600000000     C***
100700000000     C* MEMORIZZA I MESI DELL'ANNO INIZIALE E FINALE E IL RESTO A ZERO
100800000000X   1C                   ELSE
100900000000DO  2C     WMM           DOWLE     12
101000000000     C                   ADD       1             J
101100000000     C                   Z-ADD     WAA           ANN(J)
101200000000     C                   Z-ADD     WMM           MES(J)
101300000000     C                   ADD       1             WMM
101400000000E   2C                   ENDDO
101500000000     C                   Z-ADD     01            WMM
101600000000DO  2C     WMM           DOWLE     WMM2
101700000000     C                   ADD       1             J
101800000000     C                   Z-ADD     WAA2          ANN(J)
101900000000     C                   Z-ADD     WMM           MES(J)
102000000000     C                   ADD       1             WMM
102100000000E   2C                   ENDDO
102200000000     C                   Z-ADD     J             DEPM                           *MESI SELEZIONATI
102300000000DO  2C     J             DOWLT     12
102400000000     C                   ADD       1             J
102500000000     C                   Z-ADD     *ZEROS        ANN(J)
102600000000     C                   Z-ADD     *ZEROS        MES(J)
102700000000E   2C                   ENDDO
102800000000E   1C                   ENDIF
102900000000     C*
103000000000     C                   ENDSR
103100000000     C*------------------------------------------------------------------------*
103200000000     C* DECINT - DECODIFICA LA TESTATA E L'INTESTAZIONE
103300000000     C*------------------------------------------------------------------------*
103400000000     C     DECINT        BEGSR
103500000000     C*--------------------
103600000000     C* INTESTAZIONE
103700000000     C*--------------------
103800000000     C***
103900000000     C* DECODIFICA MESI
104000000000     C***
104100000000IF  1C     MES(1)        IFGT      *ZEROS
104200000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ
104300000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
104400990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
104500000000E   1C                   ENDIF
104600000000     C*
104700000000IF  1C     MES(2)        IFGT      *ZEROS
104800000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ
104900000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
105000990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
105100000000E   1C                   ENDIF
105200000000     C*
105300000000IF  1C     MES(3)        IFGT      *ZEROS
105400000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ
105500000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
105600990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
105700000000E   1C                   ENDIF
105800000000     C*
105900000000IF  1C     MES(4)        IFGT      *ZEROS
106000000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ
106100000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
106200990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
106300000000E   1C                   ENDIF
106400000000     C*
106500000000IF  1C     MES(5)        IFGT      *ZEROS
106600000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ
106700000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
106800990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
106900000000E   1C                   ENDIF
107000000000     C*
107100000000IF  1C     MES(6)        IFGT      *ZEROS
107200000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ
107300000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
107400990608     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
107500000000E   1C                   ENDIF
107600000000     C*
107700000000IF  1C     MES(7)        IFGT      *ZEROS
107800000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ
107900000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
108000990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
108100000000E   1C                   ENDIF
108200000000     C*
108300000000IF  1C     MES(8)        IFGT      *ZEROS
108400000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ
108500000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
108600990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
108700000000E   1C                   ENDIF
108800000000     C*
108900000000IF  1C     MES(9)        IFGT      *ZEROS
109000000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ
109100000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
109200990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
109300000000E   1C                   ENDIF
109400000000     C*
109500000000IF  1C     MES(10)       IFGT      *ZEROS
109600000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ
109700000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
109800990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
109900000000E   1C                   ENDIF
110000000000     C*
110100000000IF  1C     MES(11)       IFGT      *ZEROS
110200000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ
110300000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
110400990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
110500000000E   1C                   ENDIF
110600000000     C*
110700000000IF  1C     MES(12)       IFGT      *ZEROS
110800000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ
110900000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
111000990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
111100000000E   1C                   ENDIF
111200000000     C*
111300000000     C                   ENDSR
111400000000     C*------------------------------------------------------------------------*
111500000000     C* *INZSR - OPERAZIONI INIZIALI
111600000000     C*------------------------------------------------------------------------*
111700000000     C     *INZSR        BEGSR
111800000000     C*--------------------
111900000000     C* RICEVIMENTO PARAMETRI
112000000000     C*--------------------
112100000000     C     *ENTRY        PLIST
112200000000     C                   PARM                    KPJBA
112300000000     C                   MOVEL     KPJBU         PARAM
112400000000     C*--------------------
112500000000     C* VARIABILI RIFERITE AL DATA BASE
112600000000     C*--------------------
112700000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
112800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
112900000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
113000960927     C     *LIKE         DEFINE    WFLDIV        DEPDIV                         *DEPOSITI
113100960927     C     *LIKE         DEFINE    WFLTFP        DEPTFP
113200960927     C     *LIKE         DEFINE    WFLMES        DEPMES
113300960927     C     *LIKE         DEFINE    WFLDEC        DEPDEC
113400000000     C*--------------------
113500000000     C* CHIAVI DI LETTURA
113600000000     C*--------------------
113700000000     C* LETTURA TABEL00F - COMPLETA
113800000000     C     KEYTAB        KLIST
113900000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
114000000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
114100000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
114200000000     C*--------------------
114300000000     C* VARIABILI DI CONTROLLO
114400000000     C*--------------------
114500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
114600000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
114700000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
114800960903     C                   MOVEL     '0'           $TOUR             1            *PRIMO GIRO TESTATA
114900000000     C                   MOVEL     '0'           PASTFP            1            *PASS MEMOR FIL PART
115000000000     C                   MOVEL     '0'           PASDIV            1            *PASS MEMOR DIVISION
115100000000     C                   MOVEL     '0'           PASGEN            1            *PASS MEMOR GENERALE
115200000000     C*--------------------
115300000000     C* VARIABILI NON RIFERITE AL DATA BASE
115400000000     C*--------------------
115500000000     C                   Z-ADD     *ZEROS        CONBLO            2 0          *CONT DI BLOCCHI STA
115600000000     C                   Z-ADD     *ZEROS        DEPMM             2 0          *MESI DA ELABORARE
115700000000     C*
115800000000     C* CARICA LE TABELLE
115900000000     C                   EXSR      CARTAB
116000000000     C*
116100000000     C* CARICA I MESI DA ELABORARE
116200000000     C                   EXSR      CARMES
116300000000     C*
116400000000     C* DECODIFICA INTESTAZIONE
116500000000     C                   EXSR      DECINT
116600000000     C*
116700000000     C                   ENDSR
116800000000     O*------------------------------------------------------------------------*
116900000000**   DRI - DECODIFICHE RIGHE DI DETTAGLIO
117000000000TOTALE PARTENZE               1
117100000000TOTALE RITARDI                2
117200000000DI CUI:                       3
117300990817P.OPERATIVO/ARR RITARDO       4
117400000000AUTISTA                       5
117500000000CAUSE DI FORZA MAGGIORE       6
117600030506SEDE TRAINI - RESP. DISTRETTO 7
117700000000TRANSITI                      8
117800030506P.OPERATIVO/ARR ORARIO        9
117900990817                           (%)10
11800000000011
11810000000012
11820000000013
118300960903DIVISIONE                     14
118400960903* AREA *                      15
118500980223P.OPERATIVO                   16
118600000000DIVISIONE                     17
118700000000TOTALE                        18
118800000000GENERALE                      19
11890096090320
119000000000**   DMM - DECODIFICHE MESI DELL'ANNO
119100000000GEN  1
119200000000FEB  2
119300000000MAR  3
119400000000APR  4
119500000000MAG  5
119600000000GIU  6
119700000000LUG  7
119800000000AGO  8
119900000000SET  9
120000000000OTT  10
120100000000NOV  11
120200000000DIC  12
