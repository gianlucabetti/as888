000100970724     H*------------------------------------------------------------------------*
000200970724     H* CANCELLAZIONE WORKFILE DI SEDE - ESEGUE
000300980122     H* NB: LANCIATO DALLA CANCELLAZIONE MENSILE SALDI - TNSS42C
000400970724     H*------------------------------------------------------------------------*
000500970724     H DECEDIT('0,') DATEDIT(*DMY.)
000600970724     F*------------------------------------------------------------------------*
000700970724     F* DATA BASE
000800970724     F*------------------------------------------------------------------------*
000900000103     FWFDLA01L  UF   E           K DISK    USROPN
001000000103     FWFDLC01L  UF   E           K DISK    USROPN
001100000103     FWFDLP01L  UF   E           K DISK    USROPN
001200000103     FWFVLM01L  UF   E           K DISK    USROPN
001300000103     FWFPES01L  UF   E           K DISK    USROPN
001400011022     FTETEC01L  UF   E           K DISK    USROPN
001500970724     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600970724     D*------------------------------------------------------------------------*
001700970724     D* SCHIERE
001800970724     D*------------------------------------------------------------------------*
001900970724     D*-------------------
002000970724     D* RIGHE DI STAMPA
002100970724     D*-------------------
002200011022     D RIG             S             66    DIM(20) CTDATA PERRCD(1)
002300970724     D*------------------------------------------------------------------------*
002400970724     D* INPUT
002500970724     D*------------------------------------------------------------------------*
002600970724     D*-------------------
002700020920     D* DS REPERIMENTO DATI UTENTE
002800970724     D*-------------------
002900020920     D TIBS34DS      E DS                                                       *Profili utente
003000020920     D DDATIUTE      E DS                                                       *Dati utente
003100020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003200970724     D*-------------------
003300970724     D* DS COMPOSIZIONE DATA -WFDLA-
003400970724     D*-------------------
003500970724     D                 DS
003600970724     D  WFAANN                 1      4  0
003700970724     D  WFAMES                 5      6  0
003800990527     D  WFADAT                 1      6  0
003900970724     D*-------------------
004000970724     D* DS COMPOSIZIONE DATA -WFDLC-
004100970724     D*-------------------
004200970724     D                 DS
004300970724     D  WFCANN                 1      4  0
004400970724     D  WFCMES                 5      6  0
004500990527     D  WFCDAT                 1      6  0
004600970724     D*-------------------
004700970724     D* DS COMPOSIZIONE DATA -WFDLP-
004800970724     D*-------------------
004900970724     D                 DS
005000970724     D  WFPANN                 1      4  0
005100970724     D  WFPMES                 5      6  0
005200990528     D  WFPDAT                 1      6  0
005300980403     D*-------------------
005400000103     D* DS COMPOSIZIONE DATA -WFVLM-
005500980403     D*-------------------
005600980403     D                 DS
005700000103     D  VLMANN                 1      4  0
005800000103     D  VLMMES                 5      6  0
005900000103     D  VLMDAT                 1      6  0
006000000103     D*-------------------
006100000103     D* DS COMPOSIZIONE DATA -WFPES-
006200000103     D*-------------------
006300000103     D                 DS
006400000103     D  PESANN                 1      4  0
006500000103     D  PESMES                 5      6  0
006600000103     D  PESDAT                 1      6  0
006700011022     D*-------------------
006800011022     D* DS COMPOSIZIONE DATA -TETEC-
006900011022     D*-------------------
007000011022     D                 DS
007100011022     D  TECANN                 1      4  0
007200011022     D  TECMES                 5      6  0
007300011022     D  TECAAM                 1      6  0
007400011022     D  TECDAT                 1      8  0
007500970724     D*------------------
007600970724     D* DS PARAMETRI LANCIO
007700970724     D*------------------
007800970724     D PARAM           DS
007900990527     D  PARDAT                 1      6  0
008000990527     D  PARDA2                 7     12  0
008100970724     D*-------------------
008200970724     D* ARCHITETTURA
008300970724     D*-------------------
008400970724     D KPJBA         E DS
008500970724     C*------------------------------------------------------------------------*
008600970724     C* MAIN LINES
008700970724     C*------------------------------------------------------------------------*
008800970724     C*
008900970724     C* ELABORAZIONE PER WFDLA
009000000103     C                   OPEN      WFDLA01L
009100970724     C                   EXSR      ELAWFA
009200000103     C                   CLOSE     WFDLA01L
009300970724     C*
009400970724     C* ELABORAZIONE PER WFDLC
009500000103     C                   OPEN      WFDLC01L
009600970724     C                   EXSR      ELAWFC
009700000103     C                   CLOSE     WFDLC01L
009800970724     C*
009900970724     C* ELABORAZIONE PER WFDLP
010000000103     C                   OPEN      WFDLP01L
010100970724     C                   EXSR      ELAWFP
010200000103     C                   CLOSE     WFDLP01L
010300980403     C*
010400000103     C* ELABORAZIONE PER WFVLM
010500000103     C                   OPEN      WFVLM01L
010600000103     C                   EXSR      ELAVLM
010700000103     C                   CLOSE     WFVLM01L
010800000103     C*
010900000103     C* ELABORAZIONE PER WFPES
011000000103     C                   OPEN      WFPES01L
011100000103     C                   EXSR      ELAPES
011200000103     C                   CLOSE     WFPES01L
011300011022     C*
011400011022     C* ELABORAZIONE PER TETEC
011500011022     C                   OPEN      TETEC01L
011600011022     C                   EXSR      ELATEC
011700011022     C                   CLOSE     TETEC01L
011800970724     C*
011900970724     C                   SETON                                        LR
012000970724     C*------------------------------------------------------------------------*
012100970724     C* ELAWFA - ELABORAZIONE PER WFDLA
012200970724     C*------------------------------------------------------------------------*
012300970724     C     ELAWFA        BEGSR
012400970724     C*
012500970724     C* POSIZIONAMENTO E PRIMA LETTURA
012600970724     C                   EXSR      SETWFA
012700970724     C*
012800970724     C* CICLO FINO A FINE CICLO O FINE FILE
012900970724DO  1C     $FINE         DOWEQ     'N'
013000970724     C     $FWFA         ANDEQ     'N'
013100970724     C*
013200970724     C* CANCELLA RECORD
013300970724     C                   EXSR      DELWFA
013400970724     C*
013500970724     C* LETTURA SUCESSIVA
013600970724     C                   EXSR      LETWFA
013700970724     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
013800970724E   1C                   ENDDO                                                  *FINE FILE
013900970724     C*
014000970724     C* STAMPA RIEPILOGO ELABORAZIONE
014100970724     C                   MOVEL     'A'           WWFX
014200970724     C                   EXSR      STATOT
014300970724     C*
014400970724     C                   ENDSR
014500970724     C*------------------------------------------------------------------------*
014600970724     C* SETWFA - POSIZIONAMENTO E PRIMA LETTURA
014700970724     C*------------------------------------------------------------------------*
014800970724     C     SETWFA        BEGSR
014900970724     C*
015000970724     C* REIMPOSTA FINE PGM
015100970724     C                   MOVEL     'N'           $FINE                          *FINE PGM
015200970724     C*
015300970724     C     *LOVAL        SETLL     WFDLA01L                           99
015400970724IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
015500970724     C                   MOVEL     'S'           $FWFA                          *FINE CICLO
015600970724X   1C                   ELSE
015700970724     C                   EXSR      LETWFA                                       *LETTURA RECORD
015800970724E   1C                   ENDIF
015900970724     C*
016000970724     C                   ENDSR
016100970724     C*------------------------------------------------------------------------*
016200970724     C* LETWFA - LETTURA PROSSIMO RECORD
016300970724     C*------------------------------------------------------------------------*
016400970724     C     LETWFA        BEGSR
016500970724     C*
016600970724     C* LETTURA FINO A:
016700970724     C*  - FINE FILE
016800970724     C*  - TROVATO RECORD VALIDO
016900970724     C                   MOVEL     'N'           WRECOK
017000970724DO  1C     $FWFA         DOWEQ     'N'
017100970724     C     WRECOK        ANDEQ     'N'
017200970724     C                   READ      WFDLA01L                               99
017300970724IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
017400970724     C                   MOVEL     'S'           $FWFA                          *FINE CICLO
017500970724X   2C                   ELSE
017600970724     C                   ADD       1             TOLWFA                         *TOTALI LETTI
017700970724     C                   EXSR      CHKWFA                                       *CONTROLLA RECORD
017800970724E   2C                   ENDIF
017900970724E   1C                   ENDDO
018000970724     C*
018100970724     C                   ENDSR
018200970724     C*------------------------------------------------------------------------*
018300970724     C* CHKWFA - CONTROLLA VALIDITA' RECORD
018400970724     C*------------------------------------------------------------------------*
018500970724     C     CHKWFA        BEGSR
018600970724     C*
018700970724     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
018800970724     C*
018900970724     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
019000970724IF  1C     WFADAT        IFGE      PARDAT                                       *LANCIO PER SALDI
019100970724     C     WFADAT        ANDLE     PARDA2
019200970724     C     WFAANN        OREQ      *ZEROS                                       *LANCIO ESTEMPORANEO
019300970724     C     WFAMES        ANDEQ     *ZEROS
019400970724X   1C                   ELSE
019500970724     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
019600970724E   1C                   ENDIF
019700970724     C*
019800970724     C                   ENDSR
019900970724     C*------------------------------------------------------------------------*
020000970724     C* DELWFA - CANCELLA RECORD
020100970724     C*------------------------------------------------------------------------*
020200970724     C     DELWFA        BEGSR
020300970724     C*
020400970724     C                   DELETE    WFDLA                                98
020500970724IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
020600970724     C                   ADD       1             TOCWFA                         *TOTALI CANCELLATI
020700970724E   1C                   ENDIF
020800970724     C*
020900970724     C                   ENDSR
021000970724     C*------------------------------------------------------------------------*
021100970724     C* ELAWFC - ELABORAZIONE PER WFDLC
021200970724     C*------------------------------------------------------------------------*
021300970724     C     ELAWFC        BEGSR
021400970724     C*
021500970724     C* POSIZIONAMENTO E PRIMA LETTURA
021600970724     C                   EXSR      SETWFC
021700970724     C*
021800970724     C* CICLO FINO A FINE CICLO O FINE FILE
021900970724DO  1C     $FINE         DOWEQ     'N'
022000970724     C     $FWFC         ANDEQ     'N'
022100970724     C*
022200970724     C* CANCELLA RECORD
022300970724     C                   EXSR      DELWFC
022400970724     C*
022500970724     C* LETTURA SUCESSIVA
022600970724     C                   EXSR      LETWFC
022700970724     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
022800970724E   1C                   ENDDO                                                  *FINE FILE
022900970724     C*
023000970724     C* STAMPA RIEPILOGO ELABORAZIONE
023100970724     C                   MOVEL     'C'           WWFX
023200970724     C                   EXSR      STATOT
023300970724     C*
023400970724     C                   ENDSR
023500970724     C*------------------------------------------------------------------------*
023600970724     C* SETWFC - POSIZIONAMENTO E PRIMA LETTURA
023700970724     C*------------------------------------------------------------------------*
023800970724     C     SETWFC        BEGSR
023900970724     C*
024000970724     C* REIMPOSTA FINE PGM
024100970724     C                   MOVEL     'N'           $FINE                          *FINE PGM
024200970724     C*
024300970724     C     *LOVAL        SETLL     WFDLC01L                           99
024400970724IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
024500970724     C                   MOVEL     'S'           $FWFC                          *FINE CICLO
024600970724X   1C                   ELSE
024700970724     C                   EXSR      LETWFC                                       *LETTURA RECORD
024800970724E   1C                   ENDIF
024900970724     C*
025000970724     C                   ENDSR
025100970724     C*------------------------------------------------------------------------*
025200970724     C* LETWFC - LETTURA PROSSIMO RECORD
025300970724     C*------------------------------------------------------------------------*
025400970724     C     LETWFC        BEGSR
025500970724     C*
025600970724     C* LETTURA FINO A:
025700970724     C*  - FINE FILE
025800970724     C*  - TROVATO RECORD VALIDO
025900970724     C                   MOVEL     'N'           WRECOK
026000970724DO  1C     $FWFC         DOWEQ     'N'
026100970724     C     WRECOK        ANDEQ     'N'
026200970724     C                   READ      WFDLC01L                               99
026300970724IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
026400970724     C                   MOVEL     'S'           $FWFC                          *FINE CICLO
026500970724X   2C                   ELSE
026600970724     C                   ADD       1             TOLWFC                         *TOTALI LETTI
026700970724     C                   EXSR      CHKWFC                                       *CONTROLLA RECORD
026800970724E   2C                   ENDIF
026900970724E   1C                   ENDDO
027000970724     C*
027100970724     C                   ENDSR
027200970724     C*------------------------------------------------------------------------*
027300970724     C* CHKWFC - CONTROLLA VALIDITA' RECORD
027400970724     C*------------------------------------------------------------------------*
027500970724     C     CHKWFC        BEGSR
027600970724     C*
027700970724     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
027800970724     C*
027900970724     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
028000970724IF  1C     WFCDAT        IFGE      PARDAT                                       *LANCIO PER SALDI
028100970724     C     WFCDAT        ANDLE     PARDA2
028200970724     C     WFCANN        OREQ      *ZEROS                                       *LANCIO ESTEMPORANEO
028300970724     C     WFCMES        ANDEQ     *ZEROS
028400970724X   1C                   ELSE
028500970724     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028600970724E   1C                   ENDIF
028700970724     C*
028800970724     C                   ENDSR
028900970724     C*------------------------------------------------------------------------*
029000970724     C* DELWFC - CANCELLA RECORD
029100970724     C*------------------------------------------------------------------------*
029200970724     C     DELWFC        BEGSR
029300970724     C*
029400970724     C                   DELETE    WFDLC                                98
029500970724IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
029600970724     C                   ADD       1             TOCWFC                         *TOTALI CANCELLATI
029700970724E   1C                   ENDIF
029800970724     C*
029900970724     C                   ENDSR
030000970724     C*------------------------------------------------------------------------*
030100970724     C* ELAWFP - ELABORAZIONE PER WFDLP
030200970724     C*------------------------------------------------------------------------*
030300970724     C     ELAWFP        BEGSR
030400970724     C*
030500970724     C* POSIZIONAMENTO E PRIMA LETTURA
030600970724     C                   EXSR      SETWFP
030700970724     C*
030800970724     C* CICLO FINO A FINE CICLO O FINE FILE
030900970724DO  1C     $FINE         DOWEQ     'N'
031000970724     C     $FWFP         ANDEQ     'N'
031100970724     C*
031200970724     C* CANCELLA RECORD
031300970724     C                   EXSR      DELWFP
031400970724     C*
031500970724     C* LETTURA SUCESSIVA
031600970724     C                   EXSR      LETWFP
031700970724     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
031800970724E   1C                   ENDDO                                                  *FINE FILE
031900970724     C*
032000970724     C* STAMPA RIEPILOGO ELABORAZIONE
032100970724     C                   MOVEL     'P'           WWFX
032200970724     C                   EXSR      STATOT
032300970724     C*
032400970724     C                   ENDSR
032500970724     C*------------------------------------------------------------------------*
032600970724     C* SETWFP - POSIZIONAMENTO E PRIMA LETTURA
032700970724     C*------------------------------------------------------------------------*
032800970724     C     SETWFP        BEGSR
032900970724     C*
033000970724     C* REIMPOSTA FINE PGM
033100970724     C                   MOVEL     'N'           $FINE                          *FINE PGM
033200970724     C*
033300000331     C     *LOVAL        SETLL     WFDLP01L                           99
033400970724IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
033500970724     C                   MOVEL     'S'           $FWFP                          *FINE CICLO
033600970724X   1C                   ELSE
033700970724     C                   EXSR      LETWFP                                       *LETTURA RECORD
033800970724E   1C                   ENDIF
033900970724     C*
034000970724     C                   ENDSR
034100970724     C*------------------------------------------------------------------------*
034200970724     C* LETWFP - LETTURA PROSSIMO RECORD
034300970724     C*------------------------------------------------------------------------*
034400970724     C     LETWFP        BEGSR
034500970724     C*
034600970724     C* LETTURA FINO A:
034700970724     C*  - FINE FILE
034800970724     C*  - TROVATO RECORD VALIDO
034900970724     C                   MOVEL     'N'           WRECOK
035000970724DO  1C     $FWFP         DOWEQ     'N'
035100970724     C     WRECOK        ANDEQ     'N'
035200970724     C                   READ      WFDLP01L                               99
035300970724IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
035400970724     C                   MOVEL     'S'           $FWFP                          *FINE CICLO
035500970724X   2C                   ELSE
035600970724     C                   ADD       1             TOLWFP                         *TOTALI LETTI
035700970724     C                   EXSR      CHKWFP                                       *CONTROLLA RECORD
035800970724E   2C                   ENDIF
035900970724E   1C                   ENDDO
036000970724     C*
036100970724     C                   ENDSR
036200970724     C*------------------------------------------------------------------------*
036300970724     C* CHKWFP - CONTROLLA VALIDITA' RECORD
036400970724     C*------------------------------------------------------------------------*
036500970724     C     CHKWFP        BEGSR
036600970724     C*
036700970724     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
036800970724     C*
036900970724     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
037000970724IF  1C     WFPDAT        IFGE      PARDAT                                       *LANCIO PER SALDI
037100970724     C     WFPDAT        ANDLE     PARDA2
037200970724     C     WFPANN        OREQ      *ZEROS                                       *LANCIO ESTEMPORANEO
037300970724     C     WFPMES        ANDEQ     *ZEROS
037400970724X   1C                   ELSE
037500970724     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
037600970724E   1C                   ENDIF
037700970724     C*
037800970724     C                   ENDSR
037900970724     C*------------------------------------------------------------------------*
038000970724     C* DELWFP - CANCELLA RECORD
038100970724     C*------------------------------------------------------------------------*
038200970724     C     DELWFP        BEGSR
038300970724     C*
038400970724     C                   DELETE    WFDLP                                98
038500970724IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
038600970724     C                   ADD       1             TOCWFP                         *TOTALI CANCELLATI
038700970724E   1C                   ENDIF
038800970724     C*
038900970724     C                   ENDSR
039000980403     C*------------------------------------------------------------------------*
039100000103     C* ELAVLM - ELABORAZIONE PER WFVLM
039200980403     C*------------------------------------------------------------------------*
039300000103     C     ELAVLM        BEGSR
039400980403     C*
039500980403     C* POSIZIONAMENTO E PRIMA LETTURA
039600000103     C                   EXSR      SETVLM
039700980403     C*
039800980403     C* CICLO FINO A FINE CICLO O FINE FILE
039900980403DO  1C     $FINE         DOWEQ     'N'
040000000103     C     $FVLM         ANDEQ     'N'
040100980403     C*
040200980403     C* CANCELLA RECORD
040300000103     C                   EXSR      DELVLM
040400980403     C*
040500980403     C* LETTURA SUCESSIVA
040600000103     C                   EXSR      LETVLM
040700980403     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
040800980403E   1C                   ENDDO                                                  *FINE FILE
040900980403     C*
041000980403     C* STAMPA RIEPILOGO ELABORAZIONE
041100980403     C                   MOVEL     'V'           WWFX
041200980403     C                   EXSR      STATOT
041300980403     C*
041400980403     C                   ENDSR
041500980403     C*------------------------------------------------------------------------*
041600000103     C* SETVLM - POSIZIONAMENTO E PRIMA LETTURA
041700980403     C*------------------------------------------------------------------------*
041800000103     C     SETVLM        BEGSR
041900980403     C*
042000980403     C* REIMPOSTA FINE PGM
042100980403     C                   MOVEL     'N'           $FINE                          *FINE PGM
042200980403     C*
042300000103     C     *LOVAL        SETLL     WFVLM01L                           99
042400980403IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
042500000103     C                   MOVEL     'S'           $FVLM                          *FINE CICLO
042600980403X   1C                   ELSE
042700000103     C                   EXSR      LETVLM                                       *LETTURA RECORD
042800980403E   1C                   ENDIF
042900980403     C*
043000980403     C                   ENDSR
043100980403     C*------------------------------------------------------------------------*
043200000103     C* LETVLM - LETTURA PROSSIMO RECORD
043300980403     C*------------------------------------------------------------------------*
043400000103     C     LETVLM        BEGSR
043500980403     C*
043600980403     C* LETTURA FINO A:
043700980403     C*  - FINE FILE
043800980403     C*  - TROVATO RECORD VALIDO
043900980403     C                   MOVEL     'N'           WRECOK
044000000103DO  1C     $FVLM         DOWEQ     'N'
044100980403     C     WRECOK        ANDEQ     'N'
044200000103     C                   READ      WFVLM01L                               99
044300000103     C                   Z-ADD     WFVANN        VLMANN
044400000103 XXX C                   Z-ADD     WFVMES        VLMMES
044500980403IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
044600000103     C                   MOVEL     'S'           $FVLM                          *FINE CICLO
044700980403X   2C                   ELSE
044800000103     C                   ADD       1             TOLVLM                         *TOTALI LETTI
044900000103     C                   EXSR      CHKVLM                                       *CONTROLLA RECORD
045000980403E   2C                   ENDIF
045100980403E   1C                   ENDDO
045200980403     C*
045300980403     C                   ENDSR
045400980403     C*------------------------------------------------------------------------*
045500000103     C* CHKVLM - CONTROLLA VALIDITA' RECORD
045600980403     C*------------------------------------------------------------------------*
045700000103     C     CHKVLM        BEGSR
045800980403     C*
045900980403     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
046000980403     C*
046100980403     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
046200000103IF  1C     VLMDAT        IFGE      PARDAT                                       *LANCIO PER SALDI
046300000103     C     VLMDAT        ANDLE     PARDA2
046400000103     C     VLMANN        OREQ      *ZEROS                                       *LANCIO ESTEMPORANEO
046500000103     C     VLMMES        ANDEQ     *ZEROS
046600980403X   1C                   ELSE
046700980403     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046800980403E   1C                   ENDIF
046900980403     C*
047000980403     C                   ENDSR
047100980403     C*------------------------------------------------------------------------*
047200000103     C* DELVLM - CANCELLA RECORD
047300980403     C*------------------------------------------------------------------------*
047400000103     C     DELVLM        BEGSR
047500980403     C*
047600000103     C                   DELETE    WFVOL                                98
047700980403IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
047800000103     C                   ADD       1             TOCVLM                         *TOTALI CANCELLATI
047900980403E   1C                   ENDIF
048000980403     C*
048100980403     C                   ENDSR
048200000103     C*------------------------------------------------------------------------*
048300000103     C* ELAPES - ELABORAZIONE PER WFPES
048400000103     C*------------------------------------------------------------------------*
048500000103     C     ELAPES        BEGSR
048600000103     C*
048700000103     C* POSIZIONAMENTO E PRIMA LETTURA
048800000103     C                   EXSR      SETPES
048900000103     C*
049000000103     C* CICLO FINO A FINE CICLO O FINE FILE
049100000103DO  1C     $FINE         DOWEQ     'N'
049200000103     C     $FPES         ANDEQ     'N'
049300000103     C*
049400000103     C* CANCELLA RECORD
049500000103     C                   EXSR      DELPES
049600000103     C*
049700000103     C* LETTURA SUCESSIVA
049800000103     C                   EXSR      LETPES
049900000103     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
050000000103E   1C                   ENDDO                                                  *FINE FILE
050100000103     C*
050200000103     C* STAMPA RIEPILOGO ELABORAZIONE
050300000103     C                   MOVEL     'S'           WWFX
050400000103     C                   EXSR      STATOT
050500000103     C*
050600000103     C                   ENDSR
050700000103     C*------------------------------------------------------------------------*
050800000103     C* SETPES - POSIZIONAMENTO E PRIMA LETTURA
050900000103     C*------------------------------------------------------------------------*
051000000103     C     SETPES        BEGSR
051100000103     C*
051200000103     C* REIMPOSTA FINE PGM
051300000103     C                   MOVEL     'N'           $FINE                          *FINE PGM
051400000103     C*
051500000103     C     *LOVAL        SETLL     WFPES01L                           99
051600000103IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
051700000103     C                   MOVEL     'S'           $FPES                          *FINE CICLO
051800000103X   1C                   ELSE
051900000103     C                   EXSR      LETPES                                       *LETTURA RECORD
052000000103E   1C                   ENDIF
052100000103     C*
052200000103     C                   ENDSR
052300000103     C*------------------------------------------------------------------------*
052400000103     C* LETPES - LETTURA PROSSIMO RECORD
052500000103     C*------------------------------------------------------------------------*
052600000103     C     LETPES        BEGSR
052700000103     C*
052800000103     C* LETTURA FINO A:
052900000103     C*  - FINE FILE
053000000103     C*  - TROVATO RECORD VALIDO
053100000103     C                   MOVEL     'N'           WRECOK
053200000103DO  1C     $FPES         DOWEQ     'N'
053300000103     C     WRECOK        ANDEQ     'N'
053400000103     C                   READ      WFPES01L                               99
053500000103     C                   Z-ADD     WFPANN        PESANN
053600000103     C                   Z-ADD     WFPMES        PESMES
053700000103IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
053800000103     C                   MOVEL     'S'           $FPES                          *FINE CICLO
053900000103X   2C                   ELSE
054000000103     C                   ADD       1             TOLPES                         *TOTALI LETTI
054100000103     C                   EXSR      CHKPES                                       *CONTROLLA RECORD
054200000103E   2C                   ENDIF
054300000103E   1C                   ENDDO
054400000103     C*
054500000103     C                   ENDSR
054600000103     C*------------------------------------------------------------------------*
054700000103     C* CHKPES - CONTROLLA VALIDITA' RECORD
054800000103     C*------------------------------------------------------------------------*
054900000103     C     CHKPES        BEGSR
055000000103     C*
055100000103     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
055200000103     C*
055300000103     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
055400000103IF  1C     PESDAT        IFGE      PARDAT                                       *LANCIO PER SALDI
055500000103     C     PESDAT        ANDLE     PARDA2
055600000103     C     PESANN        OREQ      *ZEROS                                       *LANCIO ESTEMPORANEO
055700000103     C     PESMES        ANDEQ     *ZEROS
055800000103X   1C                   ELSE
055900000103     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056000000103E   1C                   ENDIF
056100000103     C*
056200000103     C                   ENDSR
056300000103     C*------------------------------------------------------------------------*
056400000103     C* DELPES - CANCELLA RECORD
056500000103     C*------------------------------------------------------------------------*
056600000103     C     DELPES        BEGSR
056700000103     C*
056800000103     C                   DELETE    WFPES                                98
056900000103IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
057000000103     C                   ADD       1             TOCPES                         *TOTALI CANCELLATI
057100000103E   1C                   ENDIF
057200000103     C*
057300000103     C                   ENDSR
057400011022     C*------------------------------------------------------------------------*
057500011022     C* ELATEC - ELABORAZIONE PER TETEC
057600011022     C*------------------------------------------------------------------------*
057700011022     C     ELATEC        BEGSR
057800011022     C*
057900011022     C* POSIZIONAMENTO E PRIMA LETTURA
058000011022     C                   EXSR      SETTEC
058100011022     C*
058200011022     C* CICLO FINO A FINE CICLO O FINE FILE
058300011022DO  1C     $FINE         DOWEQ     'N'
058400011022     C     $FTEC         ANDEQ     'N'
058500011022     C*
058600011022     C* CANCELLA RECORD
058700011022     C                   EXSR      DELTEC
058800011022     C*
058900011022     C* LETTURA SUCESSIVA
059000011022     C                   EXSR      LETTEC
059100011022     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
059200011022E   1C                   ENDDO                                                  *FINE FILE
059300011022     C*
059400011022     C* STAMPA RIEPILOGO ELABORAZIONE
059500011022     C                   MOVEL     'E'           WWFX
059600011022     C                   EXSR      STATOT
059700011022     C*
059800011022     C                   ENDSR
059900011022     C*------------------------------------------------------------------------*
060000011022     C* SETTEC - POSIZIONAMENTO E PRIMA LETTURA
060100011022     C*------------------------------------------------------------------------*
060200011022     C     SETTEC        BEGSR
060300011022     C*
060400011022     C* REIMPOSTA FINE PGM
060500011022     C                   MOVEL     'N'           $FINE                          *FINE PGM
060600011022     C*
060700011022     C     *LOVAL        SETLL     TETEC01L                           99
060800011022IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
060900011022     C                   MOVEL     'S'           $FTEC                          *FINE CICLO
061000011022X   1C                   ELSE
061100011022     C                   EXSR      LETTEC                                       *LETTURA RECORD
061200011022E   1C                   ENDIF
061300011022     C*
061400011022     C                   ENDSR
061500011022     C*------------------------------------------------------------------------*
061600011022     C* LETTEC - LETTURA PROSSIMO RECORD
061700011022     C*------------------------------------------------------------------------*
061800011022     C     LETTEC        BEGSR
061900011022     C*
062000011022     C* LETTURA FINO A:
062100011022     C*  - FINE FILE
062200011022     C*  - TROVATO RECORD VALIDO
062300011022     C                   MOVEL     'N'           WRECOK
062400011022DO  1C     $FTEC         DOWEQ     'N'
062500011022     C     WRECOK        ANDEQ     'N'
062600011022     C                   READ      TETEC01L                               99
062700011022     C                   Z-ADD     TECDTN        TECDAT
062800011022IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
062900011022     C                   MOVEL     'S'           $FTEC                          *FINE CICLO
063000011022X   2C                   ELSE
063100011022     C                   ADD       1             TOLTEC                         *TOTALI LETTI
063200011022     C                   EXSR      CHKTEC                                       *CONTROLLA RECORD
063300011022E   2C                   ENDIF
063400011022E   1C                   ENDDO
063500011022     C*
063600011022     C                   ENDSR
063700011022     C*------------------------------------------------------------------------*
063800011022     C* CHKTEC - CONTROLLA VALIDITA' RECORD
063900011022     C*------------------------------------------------------------------------*
064000011022     C     CHKTEC        BEGSR
064100011022     C*
064200011022     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
064300011022     C*
064400011022     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
064500011022IF  1C     TECDAT        IFGE      PARDAT                                       *LANCIO PER SALDI
064600011022     C     TECDAT        ANDLE     PARDA2
064700011022X   1C                   ELSE
064800011022     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
064900011022E   1C                   ENDIF
065000011022     C*
065100011022     C                   ENDSR
065200011022     C*------------------------------------------------------------------------*
065300011022     C* DELTEC - CANCELLA RECORD
065400011022     C*------------------------------------------------------------------------*
065500011022     C     DELTEC        BEGSR
065600011022     C*
065700011022     C                   DELETE    TETEC000                             98
065800011022IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
065900011022     C                   ADD       1             TOCTEC                         *TOTALI CANCELLATI
066000011022E   1C                   ENDIF
066100011022     C*
066200011022     C                   ENDSR
066300970724     C*------------------------------------------------------------------------*
066400970724     C* STATOT - STAMPA RIEPILOGO ELABORAZIONE
066500970724     C*------------------------------------------------------------------------*
066600970724     C     STATOT        BEGSR
066700970724     C*
066800970724     C* FINE PAGINA, STAMPA TESTATA
066900970724IF  1C     *INOF         IFEQ      *ON
067000970724     C                   EXCEPT    TEST
067100970724     C                   SETOFF                                       OF
067200970724E   1C                   ENDIF
067300970724     C*
067400970724     C* STAMPA ERRORE PER -WFDLA-
067500970724IF  1C     WWFX          IFEQ      'A'
067600970724     C                   EXCEPT    TWFA
067700970724E   1C                   ENDIF
067800970724     C*
067900970724     C* STAMPA ERRORE PER -WFDLC-
068000970724IF  1C     WWFX          IFEQ      'C'
068100970724     C                   EXCEPT    TWFC
068200970724E   1C                   ENDIF
068300970724     C*
068400970724     C* STAMPA ERRORE PER -WFDLP-
068500970724IF  1C     WWFX          IFEQ      'P'
068600970724     C                   EXCEPT    TWFP
068700970724E   1C                   ENDIF
068800980403     C*
068900000103     C* STAMPA ERRORE PER -WFVLM-
069000980403IF  1C     WWFX          IFEQ      'V'
069100000103     C                   EXCEPT    TVLM
069200980403E   1C                   ENDIF
069300000103     C*
069400000103     C* STAMPA ERRORE PER -WFPES-
069500000103IF  1C     WWFX          IFEQ      'S'
069600000103     C                   EXCEPT    TPES
069700000103E   1C                   ENDIF
069800011022     C*
069900011022     C* STAMPA ERRORE PER -TETEC-
070000011022IF  1C     WWFX          IFEQ      'E'
070100011022     C                   EXCEPT    TTEC
070200011022E   1C                   ENDIF
070300970724     C*
070400970724     C                   ENDSR
070500020920     C*--------------------------------------------------------------------------------------------*
070600020920     C* REPERISCE I DATI UTENTE
070700020920     C*--------------------------------------------------------------------------------------------*
070800020920     C     REPDATIUTE    BEGSR
070900020920     C*
071000020920     C* INIZIALIZZA VARIABILI DI WRK
071100020920     C                   CLEAR                   TIBS34DS
071200020920     C                   CLEAR                   AZUTEDS
071300020920     C                   CLEAR                   DDATIUTE
071400020920     C*
071500020920     C     *DTAARA       DEFINE    §azute        azuteds
071600020920     C     *DTAARA       DEFINE    §datiute      ddatiute
071700020920     C                   IN(E)     *DTAARA
071800020920if  1C                   IF        %Error
071900020920     c                   EVAL      I34Tla = 'L'
072000020920     C                   CALL      'TIBS34R'
072100020920     C                   PARM                    Tibs34Ds
072200020920     C                   IN        *DTAARA
072300020920e   1C                   ENDIF
072400020920     C*
072500020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
072600020920     C                   MOVEL(P)  RSUT          DSFIRS           20
072700020920     C*
072800020920     C                   ENDSR
072900970724     C*------------------------------------------------------------------------*
073000970724     C* *INZSR - OPERAZIONI INIZIALI
073100970724     C*------------------------------------------------------------------------*
073200970724     C     *INZSR        BEGSR
073300970724     C*--------------------
073400970724     C* RICEVIMENTO PARAMETRI
073500970724     C*--------------------
073600970724     C     *ENTRY        PLIST
073700970724     C                   PARM                    KPJBA
073800970724     C                   MOVEL     KPJBU         PARAM
073900970724     C*--------------------
074000970724     C* VARIABILI DI CONTROLLO
074100970724     C*--------------------
074200970724     C                   MOVEL     'N'           $FINE             1            *FINE PGM
074300970724     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
074400970724     C                   MOVEL     'N'           $FWFA             1            *FINE LETTURA WFDLA
074500970724     C                   MOVEL     'N'           $FWFC             1            *FINE LETTURA WFDLC
074600970724     C                   MOVEL     'N'           $FWFP             1            *FINE LETTURA WFDLP
074700000103     C                   MOVEL     'N'           $FVLM             1            *FINE LETTURA WFVLM
074800000103     C                   MOVEL     'N'           $FPES             1            *FINE LETTURA WFPES
074900011022     C                   MOVEL     'N'           $FTEC             1            *FINE LETTURA TETEC
075000970724     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
075100990609     C                   MOVEL     *BLANKS       WWFX              1            *ERRORE
075200970724     C                   MOVEL     '1'           *INOF                          *OVERFLOW
075300970724     C                   Z-ADD     *ZEROS        TOLWFA            9 0          *TOT LETTI      WFDL
075400970724     C                   Z-ADD     *ZEROS        TOCWFA            9 0          *TOT CANCELLATI WFDL
075500970724     C                   Z-ADD     *ZEROS        TOLWFC            9 0          *TOT LETTI      WFDL
075600970724     C                   Z-ADD     *ZEROS        TOCWFC            9 0          *TOT CANCELLATI WFDL
075700970724     C                   Z-ADD     *ZEROS        TOLWFP            9 0          *TOT LETTI      WFDL
075800970724     C                   Z-ADD     *ZEROS        TOCWFP            9 0          *TOT CANCELLATI WFDL
075900000103     C                   Z-ADD     *ZEROS        TOLVLM            9 0          *TOT LETTI      WFVL
076000000103     C                   Z-ADD     *ZEROS        TOCVLM            9 0          *TOT CANCELLATI WFVL
076100000103     C                   Z-ADD     *ZEROS        TOLPES            9 0          *TOT LETTI      WFPE
076200000103     C                   Z-ADD     *ZEROS        TOCPES            9 0          *TOT CANCELLATI WFPE
076300011022     C                   Z-ADD     *ZEROS        TOLTEC            9 0          *TOT LETTI      TETE
076400011022     C                   Z-ADD     *ZEROS        TOCTEC            9 0          *TOT CANCELLATI TETE
076500970724     C*
076600970724     C* REPERISCE I DATI DELL'UTENTE
076700020920     C                   EXSR      REPDATIUTE
076800970724     C*
076900970724     C* REPERIMENTO DATA E ORA CORRENTE
077000970724     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
077100970724     C                   MOVE      WN14          DATCOR            8 0          *DATA (8) IN G/M/AA
077200970724     C                   MOVEL     WN14          ORACOR            6 0          *ORA (6)
077300970724     C*
077400970724     C                   ENDSR
077500970724     O*------------------------------------------------------------------------*
077600970724     OQSYSPRT   E            TEST             02
077700970724     O                       RIG(1)              66
077800970724     O                       RIG(2)             132
077900970724     O                       DSFIRS              20
078000970724     O                       DATCOR             107 '  /  /    '
078100970724     O                       ORACOR             118 '  :  :  '
078200970724     O                       PAGE          Z    132
078300970724     O*------------------------
078400970724     O          E            TEST             03
078500970724     O                       RIG(3)              66
078600970724     O                       RIG(3)             132
078700970724     O*------------------------
078800970724     O          E            TWFA        1
078900970724     O                       RIG(4)              66
079000970724     O                       RIG(5)             132
079100970724     O                       TOLWFA        Z     38
079200970724     O                       TOCWFA        Z    104
079300970724     O*------------------------
079400970724     O          E            TWFC        1
079500970724     O                       RIG(6)              66
079600970724     O                       RIG(7)             132
079700970724     O                       TOLWFC        Z     38
079800970724     O                       TOCWFC        Z    104
079900970724     O*------------------------
080000970724     O          E            TWFP        1
080100970724     O                       RIG(8)              66
080200970724     O                       RIG(9)             132
080300970724     O                       TOLWFP        Z     38
080400970724     O                       TOCWFP        Z    104
080500980403     O*------------------------
080600000103     O          E            TVLM        1
080700980403     O                       RIG(10)             66
080800980403     O                       RIG(11)            132
080900000103     O                       TOLVLM        Z     38
081000000103     O                       TOCVLM        Z    104
081100000103     O*------------------------
081200000103     O          E            TPES        1
081300000103     O                       RIG(12)             66
081400000103     O                       RIG(13)            132
081500000103     O                       TOLPES        Z     38
081600000103     O                       TOCPES        Z    104
081700011022     O*------------------------
081800011022     O          E            TTEC        1
081900011022     O                       RIG(16)             66
082000011022     O                       RIG(17)            132
082100011022     O                       TOLTEC        Z     38
082200011022     O                       TOCTEC        Z    104
082300970724     O*------------------------------------------------------------------------*
082400011022** RIG - RIGHE DI STAMPA
082500970724XXXXXXXXXXXXXXXXXXXX            TNSS49R               *** CANCELLA1
082600970724ZIONE WFILE DI SEDE ***        99/99/9999   66:66:66      PAG.99992
082700970724------------------------------------------------------------------3
082800970724RECORD LETTI     IN WFDLA -> 999999999                            4
082900970724RECORD CANELLATI IN WFDLA -> 999999999                            5
083000970724RECORD LETTI     IN WFDLC -> 999999999                            6
083100970724RECORD CANELLATI IN WFDLC -> 999999999                            7
083200970724RECORD LETTI     IN WFDLP -> 999999999                            8
083300970724RECORD CANELLATI IN WFDLP -> 999999999                            9
083400000103RECORD LETTI     IN WFVLM -> 999999999                            10
083500000103RECORD CANELLATI IN WFVLM -> 999999999                            11
083600000103RECORD LETTI     IN WFPES -> 999999999                            12
083700000103RECORD CANELLATI IN WFPES -> 999999999                            13
083800050706RECORD LETTI     IN TETEC -> 999999999                            14
083900050706RECORD CANELLATI IN TETEC -> 999999999                            15
084000050706                                                                  16
084100050706                                                                  17
084200050706                                                                  18
084300011022                                                                  19
084400011022                                                                  20
