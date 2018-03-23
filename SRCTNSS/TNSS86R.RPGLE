000100960213     H*------------------------------------------------------------------------*
000200960213     H* STATISTICA TRAINI - RICHIESTA PARAMETRI                                *
000300960213     H*------------------------------------------------------------------------*
000400960213     H DECEDIT('0,') DATEDIT(*DMY.)
000500960213     F*------------------------------------------------------------------------*
000600960213     F* DATA BASE
000700960213     F*------------------------------------------------------------------------*
000800960213     FTNSS86D   CF   E             WORKSTN INFDS(DEVDS1)
000900960213     F                                     SFILE(SS86DVM:MSGULT)
001000960213     D*------------------------------------------------------------------------*
001100960213     D* SCHIERE
001200960213     D*------------------------------------------------------------------------*
001300960213     D*-------------------
001400960213     D* MESSAGGI E FUNZIONI
001500960213     D*-------------------
001600960926     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             *MESSAGGI
001700960213     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001800960213     D*-------------------
001900960213     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000960213     D*-------------------
002100960213     D FFU             S              6    DIM(31)                              *FUNZIONE
002200960213     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002300960213     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400960213     D*-------------------
002500960213     D* TABELLA DESCRIZIONI A VIDEO
002600960213     D*-------------------
002700960213     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002800960213     D*-------------------
002900960213     D* TABELLA TASTI FUNZIONALI PREMUTI
003000960213     D*-------------------
003100960213     D KEYS            S              1    DIM(40)
003200960213     D*------------------------------------------------------------------------*
003300960213     D* INPUT
003400960213     D*------------------------------------------------------------------------*
003500960213     D*-------------------
003600960213     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700960213     D*-------------------
003800960213     D DSFUN         E DS
003900960213     D  FUNRIG                 1     40
004000960213     D*-------------------
004100960213     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004200960213     D*-------------------
004300960213     D DSMSG         E DS
004400960213     D*-------------------
004500960213     D* DS PER GESTIONE TASTI FUNZIONALI
004600960213     D*-------------------
004700960213     D DSKEYS        E DS
004800960213     D  KEYRIG                 1     40
004900960213     D*-------------------
005000960213     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100960213     D*-------------------
005200960213     D                SDS
005300960213     D  USER                 254    263
005400960213     D*-------------------
005500960213     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600960213     D*-------------------
005700960213     D DEVDS1          DS
005800960213     D  KEY                  369    369
005900960213     D  CURSOR               370    371
006000960213     D*-------------------
006100960213     D* ARCHITETTURA
006200960213     D*-------------------
006300960213     D KPJBA         E DS
006400960213     D*-------------------
006500020920     D* DS REPERIMENTO DATI UTENTE
006600020920     D*---------------------
006700020920     D TIBS34DS      E DS                                                       *Profili utente
006800020920     D DDATIUTE      E DS                                                       *Dati utente
006900020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000960213     D*-------------------
007100960213     D* DS PARAMETRI LANCIO
007200960213     D*-------------------
007300960213     D PARAM           DS                  INZ
007400990608     D  PARDAT                 1      6  0
007500990608     D  PARDA2                 7     12  0
007600990608     D  PARCRE                13     13
007700000706     D  PARPOS                14     14
007800990608     D*------------------
007900990608     D* DS CONTROLLO DATA (8)
008000990608     D*------------------
008100990608     D WLBDA8          DS                  INZ
008200990608     D  G08DAT                 1      8  0
008300990608     D  G08INV                 9     16  0
008400990608     D  G08ERR                17     17
008500990608     D  G08TGI                18     22  0
008600990608     D*------------------
008700990608     D* DS DATA GENERICA(8) IN GG/MM/AAAA
008800990608     D*------------------
008900990608     D                 DS
009000990608     D  DGIOR8                 1      2  0
009100990608     D  DMESE8                 3      4  0
009200990608     D  DANNO8                 5      8  0
009300990608     D  DDATA8                 1      8  0
009400960213     C*------------------------------------------------------------------------*
009500960213     C* MAIN LINE
009600960213     C*------------------------------------------------------------------------*
009700960213     C*
009800960213     C* CICLO FINO A CHE:
009900960213DO  1C     $FINE         DOUEQ     '1'
010000960213     C*
010100960213     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
010200960213     C*
010300960213     C* CONTROLLA GESTIONE VIDEO
010400960213IF  2C     $GES          IFEQ      'V1'
010500960213     C                   EXSR      GESV1                                        *PARAMETRI
010600960213E   2C                   ENDIF
010700960213     C*
010800960213E   1C                   ENDDO
010900960213     C*
011000960213     C                   SETON                                        LR
011100960213     C*------------------------------------------------------------------------*
011200960213     C* GESV1 - GESTIONE VIDEO UNO
011300960213     C*------------------------------------------------------------------------*
011400960213     C     GESV1         BEGSR
011500960213     C*
011600960213IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011700960213     C                   EXSR      CARV1                                        *CARICA VIDEO
011800960213E   1C                   ENDIF
011900960213     C*
012000960213IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
012100960213     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
012200960213     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
012300960213E   1C                   ENDIF
012400960213     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
012500960213     C*
012600960213     C                   EXFMT     SS86DV1                                      *PARAMETRI
012700960213     C*
012800960213     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012900960213     C*
013000960213IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
013100960213SL  2C                   SELECT
013200960213     C     KEY           WHENEQ    ENTER
013300960213     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013400960213     C     KEY           WHENEQ    F6
013500960213     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
013600960213     C                   OTHER
013700960213     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013800960213E   2C                   ENDSL
013900960213E   1C                   ENDIF
014000960213     C*
014100960213     C                   ENDSR
014200960213     C*------------------------------------------------------------------------*
014300960213     C* CARV1 - CARICA VIDEO UNO
014400960213     C*------------------------------------------------------------------------*
014500960213     C     CARV1         BEGSR
014600960213     C*
014700960213     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014800960213     C*
014900960213     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015000960213     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015100960213     C*
015200960213     C                   WRITE     SS86DVT                                      *TESTATA
015300960213     C                   WRITE     SS86DVF                                      *FUNZIONI
015400960213     C*
015500960213     C                   ENDSR
015600960213     C*------------------------------------------------------------------------*
015700960213     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
015800960213     C*------------------------------------------------------------------------*
015900960213     C     ENTV1         BEGSR
016000960213     C*
016100960213     C* CONTROLLA I CAMPI IMMESSI
016200960213     C                   EXSR      DECODI
016300960213     C*
016400960213     C                   ENDSR
016500960213     C*------------------------------------------------------------------------*
016600960213     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
016700960213     C*------------------------------------------------------------------------*
016800960213     C     F06GES        BEGSR
016900960213     C*
017000960213     C* CONTROLLA I CAMPI IMMESSI
017100960213     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
017200960213     C*
017300960213     C* LANCIA IL PROGRAMMA DI STAMPA
017400960213IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
017500960213     C                   Z-ADD     V1CDAT        PARDAT                         *AA/MM INIZIALE
017600960213     C                   Z-ADD     V1CDA2        PARDA2                         *AA/MM INIZIALE
017700960926     C                   MOVEL     V1CCRE        PARCRE                         *CREAZIONE WFILE
017800000706     C                   MOVEL     V1CPOS        PARPOS                         *DATI POSTE
017900960213     C                   MOVEL     PARAM         KPJBU
018000960213     C                   MOVE      'SS87'        KCOAZ
018100960213     C                   CALL      'BCH10'
018200960213     C                   PARM                    KPJBA
018300960213     C*
018400960213     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
018500960213E   1C                   ENDIF
018600960213     C*
018700960213     C                   ENDSR
018800960213     C*------------------------------------------------------------------------*
018900960213     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019000960213     C*------------------------------------------------------------------------*
019100960213     C     DECODI        BEGSR
019200960213     C*
019300960213     C* IMPOSTA INDICATORI DI ERRORE SUI CAMPI
019400960926     C                   SETOFF                                       606162
019500000706     C                   SETOFF                                       63
019600960213     C                   Z-ADD     *ZEROS        RISUL             4 0
019700960213     C*--------------------
019800960213     C* ANNO/MESE INIZIALE
019900960213     C*--------------------
020000960213IF  1C     $ERR          IFEQ      '0'
020100990608     C                   MOVEL     V1CDAT        G08INV
020200990608     C                   MOVE      01            G08INV
020300990608     C                   Z-ADD     *ZEROS        G08DAT
020400990608     C                   MOVEL     '3'           G08ERR
020500990608     C                   CALL      'XSRDA8'
020600990608     C                   PARM                    WLBDA8
020700990608IF  2C     G08ERR        IFEQ      '1'
020800960213     C                   MOVEL     '1'           $ERR
020900960213     C                   MOVEL     MSG(7)        $MSG
021000960213     C                   EXSR      CARMSG
021100960213     C                   SETON                                        60
021200960213E   2C                   ENDIF
021300960213E   1C                   ENDIF
021400960213     C*--------------------
021500960213     C* ANNO/MESE FINALE
021600960213     C*--------------------
021700960213IF  1C     $ERR          IFEQ      '0'
021800990608     C                   MOVEL     V1CDA2        G08INV
021900990608     C                   MOVE      01            G08INV
022000990608     C                   Z-ADD     *ZEROS        G08DAT
022100990608     C                   MOVEL     '3'           G08ERR
022200990608     C                   CALL      'XSRDA8'
022300990608     C                   PARM                    WLBDA8
022400990608IF  2C     G08ERR        IFEQ      '1'
022500960213     C                   MOVEL     '1'           $ERR
022600960213     C                   MOVEL     MSG(8)        $MSG
022700960213     C                   EXSR      CARMSG
022800960213     C                   SETON                                        61
022900960213E   2C                   ENDIF
023000960213E   1C                   ENDIF
023100960213     C*
023200960213     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO CORRENTE (SOLO INFORMATIVO)
023300960213IF  1C     $ERR          IFEQ      '0'
023400960213IF  2C     V1CDA2        IFGT      AMCOR
023500960213     C                   MOVEL     MSG(11)       $MSG
023600960213     C                   EXSR      CARMSG
023700960213E   2C                   ENDIF
023800960213E   1C                   ENDIF
023900960213     C*--------------------
024000960213     C* ANNO/MESE INIZIALE E FINALE
024100960213     C*--------------------
024200960213     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
024300960213IF  1C     $ERR          IFEQ      '0'
024400960213IF  2C     V1CDA2        IFLT      V1CDAT
024500960213     C                   MOVEL     '1'           $ERR
024600960213     C                   MOVEL     MSG(9)        $MSG
024700960213     C                   EXSR      CARMSG
024800960213     C                   SETON                                        61
024900960213E   2C                   ENDIF
025000960213E   1C                   ENDIF
025100960213     C*
025200960213     C* RICHIEDERE NON PIU' DI DODICI MESI
025300960213IF  1C     $ERR          IFEQ      '0'
025400960213     C     V1CDA2        SUB       V1CDAT        RISUL
025500960213IF  2C     RISUL         IFGE      100
025600960213     C                   MOVEL     '1'           $ERR
025700960213     C                   MOVEL     MSG(10)       $MSG
025800960213     C                   EXSR      CARMSG
025900960213     C                   SETON                                        6061
026000960213E   2C                   ENDIF
026100960213E   1C                   ENDIF
026200000706     C*--------------------
026300000706     C* CREAZIONE FILE DI LAVORO
026400000706     C*--------------------
026500000706IF  1C     $ERR          IFEQ      '0'
026600000706IF  2C     V1CCRE        IFEQ      'S'
026700000706     C     V1CCRE        OREQ      'N'
026800000706X   2C                   ELSE
026900000706     C                   MOVEL     '1'           $ERR
027000000706     C                   MOVEL     MSG(14)       $MSG
027100000706     C                   EXSR      CARMSG
027200000706     C                   SETON                                        62
027300000706E   2C                   ENDIF
027400000706E   1C                   ENDIF
027500000706     C*--------------------
027600000706     C* DATI POSTE
027700000706     C*--------------------
027800000706IF  1C     $ERR          IFEQ      '0'
027900000706IF  2C     V1CPOS        IFEQ      'S'
028000000706     C     V1CPOS        OREQ      'N'
028100000706X   2C                   ELSE
028200000706     C                   MOVEL     '1'           $ERR
028300000706     C                   MOVEL     MSG(12)       $MSG
028400000706     C                   EXSR      CARMSG
028500000706     C                   SETON                                        63
028600000706E   2C                   ENDIF
028700000706E   1C                   ENDIF
028800960213     C*
028900960213     C                   ENDSR
029000960213     C*------------------------------------------------------------------------*
029100960213     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
029200960213     C*------------------------------------------------------------------------*
029300960213     C     IMPVID        BEGSR
029400960213     C*
029500990608     C*--------------------
029600990608     C* D A T A  F I N E --> ANNO/MESE CORRENTE - 1 MESE
029700990608     C*--------------------
029800990608     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
029900990608     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
030000990608     C                   Z-ADD     N8            DDATA8                         *SCOMPOSIZIONE DATA
030100990608     C*
030200990608     C                   Z-ADD     DANNO8        ANNO              4 0
030300990608     C                   Z-ADD     DMESE8        MESE              2 0
030400990608     C                   SUB       1             MESE
030500990608IF  1C     MESE          IFEQ      *ZEROS                                       *SE GENNAIO
030600990608     C                   Z-ADD     12            MESE                           *IMPOSTO DICEMBRE
030700990608     C                   SUB       1             ANNO                           *DELL'ANNO PRECEDENT
030800990608E   1C                   ENDIF
030900990608     C                   MOVEL     ANNO          V1CDA2
031000990608     C                   MOVE      MESE          V1CDA2
031100990608     C*--------------------
031200990608     C* D A T A  I N I Z I O --> INIIZO DELL'ANNO
031300990608     C*--------------------
031400990608     C                   Z-ADD     V1CDA2        V1CDAT
031500990608     C                   MOVE      01            V1CDAT
031600990608     C*--------------------
031700990608     C* ALTRE SELEZIONI
031800990608     C*--------------------
031900960926     C                   MOVEL     'S'           V1CCRE                         *CREAZIONE WORKFILE
032000000706     C                   MOVEL     'N'           V1CPOS                         *DATI POSTE
032100990608     C*--------------------
032200990608     C* DATA CORRENTE
032300990610     C*--------------------
032400990610     C                   Z-ADD     V1CDA2        AMCOR             6 0          *ANN/MESE CORRENTE
032500960213     C*
032600960213     C                   ENDSR
032700020920     C*--------------------------------------------------------------------------------------------*
032800020920     C* REPERISCE I DATI UTENTE
032900020920     C*--------------------------------------------------------------------------------------------*
033000020920     C     REPDATIUTE    BEGSR
033100020920     C*
033200020920     C* INIZIALIZZA VARIABILI DI WRK
033300020920     C                   CLEAR                   TIBS34DS
033400020920     C                   CLEAR                   AZUTEDS
033500020920     C                   CLEAR                   DDATIUTE
033600020920     C*
033700020920     C     *DTAARA       DEFINE    §azute        azuteds
033800020920     C     *DTAARA       DEFINE    §datiute      ddatiute
033900020920     C                   IN(E)     *DTAARA
034000020920if  1C                   IF        %Error
034100020920     c                   EVAL      I34Tla = 'L'
034200020920     C                   CALL      'TIBS34R'
034300020920     C                   PARM                    Tibs34Ds
034400020920     C                   IN        *DTAARA
034500020920e   1C                   ENDIF
034600020920     C*
034700020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
034800020920     C                   MOVEL(P)  RSUT          DSFIRS           20
034900020920     C*
035000020920     C                   ENDSR
035100960213     C*------------------------------------------------------------------------*
035200960213     C* *INZSR  - ROUTINE INIZIALE
035300960213     C*------------------------------------------------------------------------*
035400960213     C     *INZSR        BEGSR
035500960213     C*--------------------
035600960213     C* RICEVIMENTO PARAMETRI
035700960213     C*--------------------
035800960213     C     *ENTRY        PLIST
035900960213     C                   PARM                    KPJBA
036000960213     C*
036100960213     C* ESEGUE OPERAZIONI INIZIALI STANDARD
036200960213     C                   EXSR      INZSTD
036300960213     C*
036400960213     C* IMPOSTAZIONE INIZIALE VIDEO
036500960213     C                   EXSR      IMPVID
036600960213     C*
036700960213     C* EMISSIONE FORMATO VIDEO
036800960213     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
036900960213     C*
037000960213     C                   ENDSR
037100960213     C*------------------------------------------------------------------------*
037200960213     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
037300960213     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
037400960213     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
037500960213     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
037600960213     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
037700960213     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
037800960213     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
037900960213     C*------------------------------------------------------------------------*
038000960213     C* INZSTD - ROUTINE INIZIALE STANDARD
038100960213     C*------------------------------------------------------------------------*
038200960213     C     INZSTD        BEGSR
038300960213     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
038400960213     C                   CALL      'XKEYS'
038500960213     C                   PARM                    DSKEYS
038600960213     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
038700960213     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
038800960213     C     XMSGPA        PLIST
038900960213     C                   PARM                    DSMSMS
039000960213     C                   PARM                    DSMSPG
039100960213     C                   PARM                    DSMSKE
039200960213     C                   PARM                    DSMSER
039300960213     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
039400960213     C                   MOVEL     'TNSS86R'     DSMSPG                         *NOME PGM CHIAMANTE
039500960213     C                   MOVEL     'TNSS86R'     VMCQUE                         *CODA MESSAGGI DESTI
039600960213     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
039700960213     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
039800960213     C* INIZIALIZZO FLAG CONTROLLO PGM
039900960213     C                   MOVEL     '0'           $ERR              1            *ERRORE
040000960213     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
040100960213     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
040200960213     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
040300960213     C                   MOVEL     '0'           $FINE             1            *FINE PGM
040400960213     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
040500960213     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
040600960213     C* LANCIO ROUTINE PER VERIFICARE SE
040700960213     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
040800960213     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
040900020920     C                   EXSR      REPDATIUTE
041000960213     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
041100960213     C                   BITOFF    '01234567'    POSMI1            1
041200960213     C                   BITOFF    '01234567'    POSMI2            1
041300960213     C                   BITOFF    '01234567'    POSMA1            1
041400960213     C                   BITOFF    '01234567'    POSMA2            1
041500960213     C                   BITON     '34'          POSMI1            1            *18
041600960213     C                   BITON     '7'           POSMI2            1            *01
041700960213     C                   BITON     '34'          POSMA1            1            *18
041800960213     C                   BITON     '0'           POSMA2            1            *80
041900960213     C                   MOVE      POSMI2        POSMIN            2
042000960213     C                   MOVEL     POSMI1        POSMIN
042100960213     C                   MOVE      POSMA2        POSMAX            2
042200960213     C                   MOVEL     POSMA1        POSMAX
042300960213     C                   ENDSR
042400960213     C*------------------------------------------------------------------------*
042500960213     C* FUNSTD - LANCIA SUBROUTINE STANDARD
042600960213     C*------------------------------------------------------------------------*
042700960213     C     FUNSTD        BEGSR
042800960213     C                   SELECT
042900960213     C     KEY           WHENEQ    F3
043000960213     C                   EXSR      F03STD
043100960213     C     KEY           WHENEQ    F12
043200960213     C                   EXSR      F12STD
043300960213     C     KEY           WHENEQ    F24
043400960213     C                   EXSR      F24STD
043500960213     C                   OTHER
043600960213     C                   MOVEL     MSG(2)        $MSG
043700960213     C                   EXSR      CARMSG
043800960213     C                   ENDSL
043900960213     C                   ENDSR
044000960213     C*------------------------------------------------------------------------*
044100960213     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
044200960213     C*------------------------------------------------------------------------*
044300960213     C     F03STD        BEGSR
044400960213     C*
044500960213     C* IMPOSTO FLAG DI FINE PROGRAMMA
044600960213     C                   MOVEL     '1'           $FINE
044700960213     C*
044800960213     C                   ENDSR
044900960213     C*------------------------------------------------------------------------*
045000960213     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
045100960213     C*------------------------------------------------------------------------*
045200960213     C     F12STD        BEGSR
045300960213     C*
045400960213     C* IMPOSTO FLAG DI FINE PROGRAMMA
045500960213     C                   MOVEL     '1'           $FINE
045600960213     C*
045700960213     C                   ENDSR
045800960213     C*------------------------------------------------------------------------*
045900960213     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
046000960213     C*------------------------------------------------------------------------*
046100960213     C     F24STD        BEGSR
046200960213     C*
046300960213     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
046400960213     C                   EXSR      OUTFUN
046500960213     C*
046600960213     C                   ENDSR
046700960213     C*------------------------------------------------------------------------*
046800960213     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
046900960213     C*          CODA MESSAGGI DEL PGM
047000960213     C*------------------------------------------------------------------------*
047100960213     C     CARMSG        BEGSR
047200960213     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
047300960213     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
047400960213     C     MSGULT        IFEQ      0
047500960213     C                   MOVEL     '1'           *IN40
047600960213     C                   WRITE     SS86DVK
047700960213     C                   MOVEL     '0'           *IN40
047800960213     C                   ENDIF
047900960213     C* INVIA MESSAGGIO ALLA CODA DEL PGM
048000960213     C                   MOVEL     $MSG          DSMSMS
048100960213     C                   CALL      'XMSG'        XMSGPA
048200960213     C     DSMSER        IFEQ      0
048300960213     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
048400960213     C                   ENDIF
048500960213     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
048600960213     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
048700960213     C                   WRITE     SS86DVM                                      *SFL MESS
048800960213     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
048900960213     C                   MOVEL     *BLANKS       $MSG
049000960213     C                   ENDSR
049100960213     C*------------------------------------------------------------------------*
049200960213     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
049300960213     C*------------------------------------------------------------------------*
049400960213     C     OUTMSG        BEGSR
049500960213     C* VISUALIZZAZIONE MESSAGGIO
049600960213     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
049700960213     C                   WRITE     SS86DVK                                      *CONTR MESS
049800960213     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
049900960213     C                   Z-ADD     *ZEROS        MSGULT
050000960213     C                   ENDSR
050100960213     C*------------------------------------------------------------------------*
050200960213     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
050300960213     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
050400960213     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
050500960213     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
050600960213     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
050700960213     C*------------------------------------------------------------------------*
050800960213     C     CARFUN        BEGSR
050900960213     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
051000960213     C     1             DO        31            I                 5 0          --- 1 -->
051100960213     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
051200960213     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
051300960213     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
051400020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
051500960213     C     $TGES         IFEQ      'G'                                          *GESTIONE
051600960213     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
051700960213     C                   ELSE                                                   *INTERROG.
051800960213     C                   MOVEL     FUNASI        FAT(I)
051900960213     C                   ENDIF
052000960213     C                   ELSE                                                   <-- 2 --> *FILIALE
052100960213     C     $TGES         IFEQ      'G'                                          *GESTIONE
052200960213     C                   MOVEL     FUNAFG        FAT(I)
052300960213     C                   ELSE                                                   *INTERROG.
052400960213     C                   MOVEL     FUNAFI        FAT(I)
052500960213     C                   ENDIF
052600960213     C                   ENDIF                                                  <-- 2 ---
052700960213     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
052800960213     C                   MOVEL     USER          WEDP              3            *PROFILO
052900960213     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
053000960213     C                   MOVEL     'S'           FAT(I)                         *ABILITO
053100960213     C                   ELSE
053200960213     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
053300960213     C                   ENDIF
053400960213     C                   ENDIF
053500960213     C                   ENDDO                                                  <-- 1 ---
053600960213     C                   ENDSR
053700960213     C*------------------------------------------------------------------------*
053800960213     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
053900960213     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
054000960213     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
054100960213     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
054200960213     C*------------------------------------------------------------------------*
054300960213     C     OUTFUN        BEGSR
054400960213     C     1             DO        8             I                              *PULIZIA FUNZIONI A
054500960213     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
054600960213     C                   ENDDO
054700960213     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
054800960213     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
054900960213     C     1             DO        31            I
055000960213     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
055100960213     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
055200960213     C                   ADD       1             FUNNAT
055300960213     C                   ENDIF
055400960213     C                   ENDDO
055500960213     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
055600960213     C     FUNNAT        IFGT      8
055700960213     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
055800960213     C                   ENDIF
055900960213     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
056000960213     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
056100960213     C                   Z-ADD     8             FUNVIS            5 0
056200960213     C     FAT(23)       IFEQ      'S'
056300960213     C                   SUB       1             FUNVIS
056400960213     C                   ENDIF
056500960213     C     FAT(24)       IFEQ      'S'
056600960213     C                   SUB       1             FUNVIS
056700960213     C                   ENDIF
056800960213     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
056900960213     C                   Z-ADD     *ZEROS        W                 5 0
057000960213     C                   ADD       1             FUNULT
057100960213     C     FUNULT        IFGT      31
057200960213     C                   Z-ADD     1             FUNULT
057300960213     C                   ENDIF
057400960213     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
057500960213     C     FUNULT        DO        31            I                              --- 1 -->
057600960213     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
057700960213     C     W             IFLT      FUNVIS                                       --- 2 -->
057800960213     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
057900960213     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
058000960213     C     FDE(I)        ANDNE     *BLANKS
058100960213     C     I             IFEQ      23                                           *TOLGO F23
058200960213     C     I             OREQ      24                                           *E F24
058300960213     C                   ELSE
058400960213     C                   ADD       1             W
058500960213     C                   MOVEL     FDE(I)        FUV(W)
058600960213     C                   ENDIF
058700960213     C                   ENDIF                                                  <-- 3 ---
058800960213     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
058900960213     C                   Z-ADD     I             FUNULT
059000960213     C                   ENDIF                                                  <-- 2 ---
059100960213     C                   ENDDO                                                  <-- 1 ---
059200960213     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
059300960213     C                   MOVEL     FUV(1)        VFD001
059400960213     C                   MOVEL     FUV(2)        VFD002
059500960213     C                   MOVEL     FUV(3)        VFD003
059600960213     C                   MOVEL     FUV(4)        VFD004
059700960213     C                   MOVEL     FUV(5)        VFD005
059800960213     C                   MOVEL     FUV(6)        VFD006
059900960213     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
060000960213     C     FAT(24)       ANDEQ     'N'
060100960213     C                   MOVEL     FUV(7)        VFD007
060200960213     C                   MOVEL     FUV(8)        VFD008
060300960213     C                   ELSE                                                   <-- 1 -->
060400960213     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
060500960213     C     FAT(24)       ANDEQ     'S'
060600960213     C                   MOVEL     FDE(23)       VFD007
060700960213     C                   MOVEL     FDE(24)       VFD008
060800960213     C                   ELSE                                                   <-- 2 -->
060900960213     C                   MOVEL     FUV(7)        VFD007
061000960213     C     FAT(23)       IFEQ      'S'
061100960213     C                   MOVEL     FDE(23)       VFD008
061200960213     C                   ELSE
061300960213     C                   MOVEL     FDE(24)       VFD008
061400960213     C                   ENDIF
061500960213     C                   ENDIF                                                  <-- 2 ---
061600960213     C                   ENDIF                                                  <-- 1 ---
061700960213     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
061800960213     C                   ENDSR
061900960213     C*------------------------------------------------------------------------*
062000960213     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
062100960213     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
062200960213     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
062300960213     C*                   - ERRORE SE FUNZIONE NON ATTIVA
062400960213     C*------------------------------------------------------------------------*
062500960213     C     CONFUN        BEGSR
062600960213     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
062700960213     C     1             DO        31            I                              --- 1 -->
062800960213     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
062900960213     C* CONTROLLO SE IL TASTO E' ATTIVO
063000960213     C     FAT(I)        IFNE      'S'                                          --- 3 -->
063100960213     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
063200960213     C                   EXSR      CARMSG
063300960213     C                   MOVEL     '1'           $ERR
063400960213     C                   ENDIF                                                  <-- 3 ---
063500960213     C                   ENDIF                                                  <-- 2 ---
063600960213     C                   ENDDO                                                  <-- 1 ---
063700960213     C                   ENDSR
063800960213     O*------------------------------------------------------------------------*
063900960213** ======== SCHIERA: MSG  (MESSAGGI)                       ================
064000960213TNSS86R- Tasto funzionale non valido.                                          1
064100960213TNSS86R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
064200960213TNSS86R- Opzione non valida.                                                   3
064300960213TNSS86R- Opzione abilitata ma non gestita. Avvertire CED.                      4
064400960213TNSS86R- Inizio elenco.                                                        5
064500960213TNSS86R- Fine elenco.                                                          6
064600960213TNSS86R- Anno/Mese iniziale errato                                             7
064700960213TNSS86R- Anno/Mese finale errato                                               8
064800960213TNSS86R- Anno/Mese finale inferiore all'Anno/Mese iniziale                     9
064900960213TNSS86R- Impossibile elaborare più di 12 mesi                                 10
065000960213TNSS86R- Data superiore alla data creazione saldi                             11
065100000706TNSS86R- Immettere uno dei valori possibili                                   12
065200960213TNSS86R- 13
065300960926TNSS86R- 'S' per creare il file di lavoro, 'N' per non crearlo                14
065400980403TNSS86R- Non si possono lanciare le statistiche di gruppo in SDI !!!          15
065500960213** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
065600960213F1    -                   -    NN     NN
065700960213F2    -                   -    NN     NN
065800960213F3    -F3=Fine            -    SS     SS
065900960213F4    -                   -    NN     NN
066000960213F5    -                   -    NN     NN
066100960213F6    -F6=Lancio          -    SS     SS
066200960213F7    -                   -    NN     NN
066300960213F8    -                   -    NN     NN
066400960213F9    -                   -    NN     NN
066500960213F10   -                   -    NN     NN
066600960213F11   -                   -    NN     NN
066700960213F12   -F12-Ritorno        -    SS     SS
066800960213F13   -                   -    NN     NN
066900960213F14   -                   -    NN     NN
067000960213F15   -                   -    NN     NN
067100960213F16   -                   -    NN     NN
067200960213F17   -                   -    NN     NN
067300960213F18   -                   -    NN     NN
067400960213F19   -                   -    NN     NN
067500960213F20   -                   -    NN     NN
067600960213F21   -                   -    NN     NN
067700960213F22   -                   -    NN     NN
067800960213F23   -                   -    NN     NN
067900960213F24   -F24=Altri tasti    -    NN     NN
068000960213ENTER -                   -    SS     SS
068100960213ROLLUP-                   -    NN     NN
068200960213ROLLDN-                   -    NN     NN
068300960213HELP  -                   -    NN     NN
068400960213PRINT -                   -    NN     NN
068500960213BSPACE-                   -    NN     NN
068600960213CLEAR -                   -    NN     NN
