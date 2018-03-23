000100970723     H*------------------------------------------------------------------------*
000200970724     H* CANCELLAZIONE SALDI CLIENTE DI SEDE - ESEGUE                           *
000300970723     H*------------------------------------------------------------------------*
000400970723     H DECEDIT('0,') DATEDIT(*DMY.)
000500970723     F*------------------------------------------------------------------------*
000600970723     F* DATA BASE
000700970723     F*------------------------------------------------------------------------*
000800970723     F*---
000900970723     F* FILES DA SALVARE -201-
001000970723     F*---
001100990609     FSISDA01L  UF   E           K DISK
001200990609     FSISDC01L  UF   E           K DISK
001300990609     FSISDF01L  UF   E           K DISK
001400990609     FSISDO01L  UF   E           K DISK
001500990609     FSISDP01L  UF   E           K DISK
001600990609     FSISDR01L  UF   E           K DISK
001700990609     FSISDT01L  UF   E           K DISK
001800040312     FSISDE02L  UF   E           K DISK
001900050124     Fsipes01L  UF   E           K DISK
002000050124     Fsivlm01L  UF   E           K DISK
002100970723     F*---
002200970723     F* FILES SALVATI -SALxx-
002300970723     F*---
002400990609     FSISDA00F  O  A E           K DISK
002500990609     F                                     RENAME(SISDA000:SISDAS)
002600990609     FSISDC00F  O  A E           K DISK
002700990609     F                                     RENAME(SISDC000:SISDCS)
002800990609     FSISDF00F  O  A E           K DISK
002900990609     F                                     RENAME(SISDF000:SISDFS)
003000990609     FSISDO00F  O  A E           K DISK
003100990609     F                                     RENAME(SISDO000:SISDOS)
003200990609     FSISDP00F  O  A E           K DISK
003300990609     F                                     RENAME(SISDP000:SISDPS)
003400990609     FSISDR00F  O  A E           K DISK
003500990609     F                                     RENAME(SISDR000:SISDRS)
003600990609     FSISDT00F  O  A E           K DISK
003700990609     F                                     RENAME(SISDT000:SISDTS)
003800040312     FSISDE00F  O  A E           K DISK
003900040312     F                                     RENAME(SISDE000:SISDES)
004000050124     Fsipes00F  O  A E           K DISK
004100050124     F                                     RENAME(sipes000:SIpesS)
004200050124     Fsivlm00F  O  A E           K DISK
004300050124     F                                     RENAME(sivol000:SIvlmS)
004400970723     F*---
004500970723     F* TABELLE
004600970723     F*---
004700970723     FTABEL00F  UF   E           K DISK
004800970724     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
004900970723     D*------------------------------------------------------------------------*
005000970723     D* SCHIERE
005100970723     D*------------------------------------------------------------------------*
005200970723     D*-------------------
005300970723     D* RIGHE DI STAMPA
005400970723     D*-------------------
005500050124     D RIG             S             66    DIM(24) CTDATA PERRCD(1)
005600970723     D*------------------------------------------------------------------------*
005700970723     D* INPUT
005800970723     D*------------------------------------------------------------------------*
005900970723     D*-------------------
006000020920     D* DS REPERIMENTO DATI UTENTE
006100970723     D*-------------------
006200020920     D TIBS34DS      E DS                                                       *Profili utente
006300020920     D DDATIUTE      E DS                                                       *Dati utente
006400020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006500970723     D*-------------------
006600020701     D* DS DATI STATISTICHE
006700970723     D*-------------------
006800020701     D DS5S          E DS
006900970723     D*-------------------
007000990609     D* DS COMPOSIZIONE DATA -SISDA-
007100970723     D*-------------------
007200970723     D                 DS
007300990526     D  SDAANN                 1      4  0
007400990526     D  SDAMES                 5      6  0
007500990526     D  SDADAT                 1      6  0
007600970723     D*-------------------
007700990609     D* DS COMPOSIZIONE DATA -SISDC-
007800970723     D*-------------------
007900970723     D                 DS
008000990526     D  SDCANN                 1      4  0
008100990526     D  SDCMES                 5      6  0
008200990526     D  SDCDAT                 1      6  0
008300970723     D*-------------------
008400990609     D* DS COMPOSIZIONE DATA -SISDF-
008500970723     D*-------------------
008600970723     D                 DS
008700990526     D  SDFANN                 1      4  0
008800990526     D  SDFMES                 5      6  0
008900990526     D  SDFDAT                 1      6  0
009000970723     D*-------------------
009100990609     D* DS COMPOSIZIONE DATA -SISDO-
009200970723     D*-------------------
009300970723     D                 DS
009400990526     D  SDOANN                 1      4  0
009500990526     D  SDOMES                 5      6  0
009600990526     D  SDODAT                 1      6  0
009700970723     D*-------------------
009800990609     D* DS COMPOSIZIONE DATA -SISDP-
009900970723     D*-------------------
010000970723     D                 DS
010100990526     D  SDPANN                 1      4  0
010200990526     D  SDPMES                 5      6  0
010300990526     D  SDPDAT                 1      6  0
010400970723     D*-------------------
010500990609     D* DS COMPOSIZIONE DATA -SISDR-
010600970723     D*-------------------
010700970723     D                 DS
010800990526     D  SDRANN                 1      4  0
010900990526     D  SDRMES                 5      6  0
011000990526     D  SDRDAT                 1      6  0
011100970723     D*-------------------
011200990609     D* DS COMPOSIZIONE DATA -SISDT-
011300970723     D*-------------------
011400970723     D                 DS
011500990526     D  SDTANN                 1      4  0
011600990526     D  SDTMES                 5      6  0
011700990526     D  SDTDAT                 1      6  0
011800040312     D*-------------------
011900040312     D* DS COMPOSIZIONE DATA -SISDE-
012000040312     D*-------------------
012100040312     D                 DS
012200040312     D  SDeANN                 1      4  0
012300040312     D  SDeMES                 5      6  0
012400040312     D  SDeDAT                 1      6  0
012500050124     D*-------------------
012600050124     D* DS COMPOSIZIONE DATA -SIpes-
012700050124     D*-------------------
012800050124     D                 DS
012900050124     D  pesANN                 1      4  0
013000050124     D  pesMES                 5      6  0
013100050124     D  pesDAT                 1      6  0
013200050124     D*-------------------
013300050124     D* DS COMPOSIZIONE DATA -SIvlm-
013400050124     D*-------------------
013500050124     D                 DS
013600050124     D  vlmANN                 1      4  0
013700050124     D  vlmMES                 5      6  0
013800050124     D  vlmDAT                 1      6  0
013900970723     D*------------------
014000970723     D* DS PARAMETRI LANCIO
014100970723     D*------------------
014200970723     D PARAM           DS
014300990526     D  PARDAT                 1      6  0
014400990526     D  PARDA2                 7     12  0
014500970723     D*-------------------
014600970723     D* ARCHITETTURA
014700970723     D*-------------------
014800970723     D KPJBA         E DS
014900970723     C*------------------------------------------------------------------------*
015000970723     C* MAIN LINES
015100970723     C*------------------------------------------------------------------------*
015200970723     C*
015300990609     C* ELABORAZIONE PER SISDA
015400970723     C                   EXSR      ELASDA
015500970723     C*
015600990609     C* ELABORAZIONE PER SISDC
015700970723     C                   EXSR      ELASDC
015800970723     C*
015900990609     C* ELABORAZIONE PER SISDF
016000970723     C                   EXSR      ELASDF
016100970723     C*
016200990609     C* ELABORAZIONE PER SISDO
016300970723     C                   EXSR      ELASDO
016400970723     C*
016500990609     C* ELABORAZIONE PER SISDP
016600970723     C                   EXSR      ELASDP
016700970723     C*
016800990609     C* ELABORAZIONE PER SISDR
016900970723     C                   EXSR      ELASDR
017000970723     C*
017100990609     C* ELABORAZIONE PER SISDT
017200970723     C                   EXSR      ELASDT
017300040312     C*
017400040312     C* ELABORAZIONE PER SISDE
017500040312     C                   EXSR      ELASDE
017600970723     C*
017700050124     C* ELABORAZIONE PER SIpes
017800050124     C                   EXSR      ELApes
017900050124     C*
018000050124     C* ELABORAZIONE PER SIvlm
018100050124     C                   EXSR      ELAvlm
018200050124     C*
018300970723     C* OPERAZIONI FINALI
018400970723     C                   EXSR      FINSR
018500970723     C*
018600970723     C                   SETON                                        LR
018700970723     C*------------------------------------------------------------------------*
018800990609     C* ELASDA - ELABORAZIONE PER SISDA
018900970723     C*------------------------------------------------------------------------*
019000970723     C     ELASDA        BEGSR
019100970723     C*
019200970723     C* POSIZIONAMENTO E PRIMA LETTURA
019300970723     C                   EXSR      SETSDA
019400970723     C*
019500970723     C* CICLO FINO A FINE CICLO O FINE FILE
019600970723DO  1C     $FINE         DOWEQ     'N'
019700970723     C     $FSDA         ANDEQ     'N'
019800970723     C*
019900970723     C* SCRIVE / CANCELLA RECORD
020000970723     C                   EXSR      AGGSDA
020100970723     C*
020200970723     C* LETTURA SUCESSIVA
020300970723     C                   EXSR      LETSDA
020400970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
020500970723E   1C                   ENDDO                                                  *FINE FILE
020600970723     C*
020700970723     C* STAMPA RIEPILOGO ELABORAZIONE
020800970723     C                   MOVEL     'A'           WSDX
020900970723     C                   EXSR      STATOT
021000970723     C*
021100970723     C                   ENDSR
021200970723     C*------------------------------------------------------------------------*
021300970723     C* SETSDA - POSIZIONAMENTO E PRIMA LETTURA
021400970723     C*------------------------------------------------------------------------*
021500970723     C     SETSDA        BEGSR
021600970723     C*
021700970723     C* REIMPOSTA FINE PGM
021800970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
021900970723     C*
022000990609     C     *LOVAL        SETLL     SISDA01L                           99
022100970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
022200970723     C                   MOVEL     'S'           $FSDA                          *FINE CICLO
022300970723X   1C                   ELSE
022400970723     C                   EXSR      LETSDA                                       *LETTURA RECORD
022500970723E   1C                   ENDIF
022600970723     C*
022700970723     C                   ENDSR
022800970723     C*------------------------------------------------------------------------*
022900970723     C* LETSDA - LETTURA PROSSIMO RECORD
023000970723     C*------------------------------------------------------------------------*
023100970723     C     LETSDA        BEGSR
023200970723     C*
023300970723     C* LETTURA FINO A:
023400970723     C*  - FINE FILE
023500970723     C*  - TROVATO RECORD VALIDO
023600970723     C                   MOVEL     'N'           WRECOK
023700970723DO  1C     $FSDA         DOWEQ     'N'
023800970723     C     WRECOK        ANDEQ     'N'
023900990609     C                   READ      SISDA01L                               99
024000970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
024100970723     C                   MOVEL     'S'           $FSDA                          *FINE CICLO
024200970723X   2C                   ELSE
024300970723     C                   EXSR      CHKSDA                                       *CONTROLLA RECORD
024400970723E   2C                   ENDIF
024500970723E   1C                   ENDDO
024600970723     C*
024700970723     C                   ENDSR
024800970723     C*------------------------------------------------------------------------*
024900970723     C* CHKSDA - CONTROLLA VALIDITA' RECORD
025000970723     C*------------------------------------------------------------------------*
025100970723     C     CHKSDA        BEGSR
025200970723     C*
025300970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
025400970723     C*
025500970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
025600970723IF  1C     SDADAT        IFGE      PARDAT
025700970723     C     SDADAT        ANDLE     PARDA2
025800970723X   1C                   ELSE
025900970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026000970723E   1C                   ENDIF
026100970723     C*
026200970723     C                   ENDSR
026300970723     C*------------------------------------------------------------------------*
026400970723     C* AGGSDA - SCRIVE / CANCELLA RECORD
026500970723     C*------------------------------------------------------------------------*
026600970723     C     AGGSDA        BEGSR
026700970723     C*
026800970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
026900990609     C                   WRITE     SISDAS                               98
027000970723     C*
027100970723     C* NO ERRORE, CANCELLA RECORD
027200970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
027300990609     C                   DELETE    SISDA000
027400990609     C                   ADD       1             TOTSDA                         *TOT AGGIORNATI SISD
027500970723     C*
027600970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
027700970723X   1C                   ELSE                                                   *ERRORE
027800970723     C                   MOVEL     'A'           WSDX
027900970723     C                   EXSR      STAERR
028000970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
028100970723E   1C                   ENDIF
028200970723     C*
028300970723     C                   ENDSR
028400970723     C*------------------------------------------------------------------------*
028500990609     C* ELASDC - ELABORAZIONE PER SISDC
028600970723     C*------------------------------------------------------------------------*
028700970723     C     ELASDC        BEGSR
028800970723     C*
028900970723     C* POSIZIONAMENTO E PRIMA LETTURA
029000970723     C                   EXSR      SETSDC
029100970723     C*
029200970723     C* CICLO FINO A FINE CICLO O FINE FILE
029300970723DO  1C     $FINE         DOWEQ     'N'
029400970723     C     $FSDC         ANDEQ     'N'
029500970723     C*
029600970723     C* SCRIVE / CANCELLA RECORD
029700970723     C                   EXSR      AGGSDC
029800970723     C*
029900970723     C* LETTURA SUCESSIVA
030000970723     C                   EXSR      LETSDC
030100970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
030200970723E   1C                   ENDDO                                                  *FINE FILE
030300970723     C*
030400970723     C                   MOVEL     'C'           WSDX
030500970723     C                   EXSR      STATOT
030600970723     C*
030700970723     C                   ENDSR
030800970723     C*------------------------------------------------------------------------*
030900970723     C* SETSDC - POSIZIONAMENTO E PRIMA LETTURA
031000970723     C*------------------------------------------------------------------------*
031100970723     C     SETSDC        BEGSR
031200970723     C*
031300970723     C* REIMPOSTA FINE PGM
031400970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
031500970723     C*
031600990609     C     *LOVAL        SETLL     SISDC01L                           99
031700970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
031800970723     C                   MOVEL     'S'           $FSDC                          *FINE CICLO
031900970723X   1C                   ELSE
032000970723     C                   EXSR      LETSDC                                       *LETTURA RECORD
032100970723E   1C                   ENDIF
032200970723     C*
032300970723     C                   ENDSR
032400970723     C*------------------------------------------------------------------------*
032500970723     C* LETSDC - LETTURA PROSSIMO RECORD
032600970723     C*------------------------------------------------------------------------*
032700970723     C     LETSDC        BEGSR
032800970723     C*
032900970723     C* LETTURA FINO A:
033000970723     C*  - FINE FILE
033100970723     C*  - TROVATO RECORD VALIDO
033200970723     C                   MOVEL     'N'           WRECOK
033300970723DO  1C     $FSDC         DOWEQ     'N'
033400970723     C     WRECOK        ANDEQ     'N'
033500990609     C                   READ      SISDC01L                               99
033600970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
033700970723     C                   MOVEL     'S'           $FSDC                          *FINE CICLO
033800970723X   2C                   ELSE
033900970723     C                   EXSR      CHKSDC                                       *CONTROLLA RECORD
034000970723E   2C                   ENDIF
034100970723E   1C                   ENDDO
034200970723     C*
034300970723     C                   ENDSR
034400970723     C*------------------------------------------------------------------------*
034500970723     C* CHKSDC - CONTROLLA VALIDITA' RECORD
034600970723     C*------------------------------------------------------------------------*
034700970723     C     CHKSDC        BEGSR
034800970723     C*
034900970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
035000970723     C*
035100970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
035200970723IF  1C     SDCDAT        IFGE      PARDAT
035300970723     C     SDCDAT        ANDLE     PARDA2
035400970723X   1C                   ELSE
035500970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
035600970723E   1C                   ENDIF
035700970723     C*
035800970723     C                   ENDSR
035900970723     C*------------------------------------------------------------------------*
036000970723     C* AGGSDC - SCRIVE / CANCELLA RECORD
036100970723     C*------------------------------------------------------------------------*
036200970723     C     AGGSDC        BEGSR
036300970723     C*
036400970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
036500990609     C                   WRITE     SISDCS                               98
036600970723     C*
036700970723     C* NO ERRORE, CANCELLA RECORD
036800970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
036900990609     C                   DELETE    SISDC000
037000990609     C                   ADD       1             TOTSDC                         *TOT AGGIORNATI SISD
037100970723     C*
037200970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
037300970723X   1C                   ELSE                                                   *ERRORE
037400970723     C                   MOVEL     'C'           WSDX
037500970723     C                   EXSR      STAERR
037600970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
037700970723E   1C                   ENDIF
037800970723     C*
037900970723     C                   ENDSR
038000970723     C*------------------------------------------------------------------------*
038100990609     C* ELASDF - ELABORAZIONE PER SISDF
038200970723     C*------------------------------------------------------------------------*
038300970723     C     ELASDF        BEGSR
038400970723     C*
038500970723     C* POSIZIONAMENTO E PRIMA LETTURA
038600970723     C                   EXSR      SETSDF
038700970723     C*
038800970723     C* CICLO FINO A FINE CICLO O FINE FILE
038900970723DO  1C     $FINE         DOWEQ     'N'
039000970723     C     $FSDF         ANDEQ     'N'
039100970723     C*
039200970723     C* SCRIVE / CANCELLA RECORD
039300970723     C                   EXSR      AGGSDF
039400970723     C*
039500970723     C* LETTURA SUCESSIVA
039600970723     C                   EXSR      LETSDF
039700970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
039800970723E   1C                   ENDDO                                                  *FINE FILE
039900970723     C*
040000970723     C                   MOVEL     'F'           WSDX
040100970723     C                   EXSR      STATOT
040200970723     C*
040300970723     C                   ENDSR
040400970723     C*------------------------------------------------------------------------*
040500970723     C* SETSDF - POSIZIONAMENTO E PRIMA LETTURA
040600970723     C*------------------------------------------------------------------------*
040700970723     C     SETSDF        BEGSR
040800970723     C*
040900970723     C* REIMPOSTA FINE PGM
041000970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
041100970723     C*
041200990609     C     *LOVAL        SETLL     SISDF01L                           99
041300970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
041400970723     C                   MOVEL     'S'           $FSDF                          *FINE CICLO
041500970723X   1C                   ELSE
041600970723     C                   EXSR      LETSDF                                       *LETTURA RECORD
041700970723E   1C                   ENDIF
041800970723     C*
041900970723     C                   ENDSR
042000970723     C*------------------------------------------------------------------------*
042100970723     C* LETSDF - LETTURA PROSSIMO RECORD
042200970723     C*------------------------------------------------------------------------*
042300970723     C     LETSDF        BEGSR
042400970723     C*
042500970723     C* LETTURA FINO A:
042600970723     C*  - FINE FILE
042700970723     C*  - TROVATO RECORD VALIDO
042800970723     C                   MOVEL     'N'           WRECOK
042900970723DO  1C     $FSDF         DOWEQ     'N'
043000970723     C     WRECOK        ANDEQ     'N'
043100990609     C                   READ      SISDF01L                               99
043200970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
043300970723     C                   MOVEL     'S'           $FSDF                          *FINE CICLO
043400970723X   2C                   ELSE
043500970723     C                   EXSR      CHKSDF                                       *CONTROLLA RECORD
043600970723E   2C                   ENDIF
043700970723E   1C                   ENDDO
043800970723     C*
043900970723     C                   ENDSR
044000970723     C*------------------------------------------------------------------------*
044100970723     C* CHKSDF - CONTROLLA VALIDITA' RECORD
044200970723     C*------------------------------------------------------------------------*
044300970723     C     CHKSDF        BEGSR
044400970723     C*
044500970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
044600970723     C*
044700970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
044800970723IF  1C     SDFDAT        IFGE      PARDAT
044900970723     C     SDFDAT        ANDLE     PARDA2
045000970723X   1C                   ELSE
045100970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
045200970723E   1C                   ENDIF
045300970723     C*
045400970723     C                   ENDSR
045500970723     C*------------------------------------------------------------------------*
045600970723     C* AGGSDF - SCRIVE / CANCELLA RECORD
045700970723     C*------------------------------------------------------------------------*
045800970723     C     AGGSDF        BEGSR
045900970723     C*
046000970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
046100990609     C                   WRITE     SISDFS                               98
046200970723     C*
046300970723     C* NO ERRORE, CANCELLA RECORD
046400970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
046500990609     C                   DELETE    SISDF000
046600990609     C                   ADD       1             TOTSDF                         *TOT AGGIORNATI SISD
046700970723     C*
046800970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
046900970723X   1C                   ELSE                                                   *ERRORE
047000970723     C                   MOVEL     'F'           WSDX
047100970723     C                   EXSR      STAERR
047200970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
047300970723E   1C                   ENDIF
047400970723     C*
047500970723     C                   ENDSR
047600970723     C*------------------------------------------------------------------------*
047700990609     C* ELASDO - ELABORAZIONE PER SISDO
047800970723     C*------------------------------------------------------------------------*
047900970723     C     ELASDO        BEGSR
048000970723     C*
048100970723     C* POSIZIONAMENTO E PRIMA LETTURA
048200970723     C                   EXSR      SETSDO
048300970723     C*
048400970723     C* CICLO FINO A FINE CICLO O FINE FILE
048500970723DO  1C     $FINE         DOWEQ     'N'
048600970723     C     $FSDO         ANDEQ     'N'
048700970723     C*
048800970723     C* SCRIVE / CANCELLA RECORD
048900970723     C                   EXSR      AGGSDO
049000970723     C*
049100970723     C* LETTURA SUCESSIVA
049200970723     C                   EXSR      LETSDO
049300970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
049400970723E   1C                   ENDDO                                                  *FINE FILE
049500970723     C*
049600970723     C                   MOVEL     'O'           WSDX
049700970723     C                   EXSR      STATOT
049800970723     C*
049900970723     C                   ENDSR
050000970723     C*------------------------------------------------------------------------*
050100970723     C* SETSDO - POSIZIONAMENTO E PRIMA LETTURA
050200970723     C*------------------------------------------------------------------------*
050300970723     C     SETSDO        BEGSR
050400970723     C*
050500970723     C* REIMPOSTA FINE PGM
050600970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
050700970723     C*
050800990609     C     *LOVAL        SETLL     SISDO01L                           99
050900970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
051000970723     C                   MOVEL     'S'           $FSDO                          *FINE CICLO
051100970723X   1C                   ELSE
051200970723     C                   EXSR      LETSDO                                       *LETTURA RECORD
051300970723E   1C                   ENDIF
051400970723     C*
051500970723     C                   ENDSR
051600970723     C*------------------------------------------------------------------------*
051700970723     C* LETSDO - LETTURA PROSSIMO RECORD
051800970723     C*------------------------------------------------------------------------*
051900970723     C     LETSDO        BEGSR
052000970723     C*
052100970723     C* LETTURA FINO A:
052200970723     C*  - FINE FILE
052300970723     C*  - TROVATO RECORD VALIDO
052400970723     C                   MOVEL     'N'           WRECOK
052500970723DO  1C     $FSDO         DOWEQ     'N'
052600970723     C     WRECOK        ANDEQ     'N'
052700990609     C                   READ      SISDO01L                               99
052800970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
052900970723     C                   MOVEL     'S'           $FSDO                          *FINE CICLO
053000970723X   2C                   ELSE
053100970723     C                   EXSR      CHKSDO                                       *CONTROLLA RECORD
053200970723E   2C                   ENDIF
053300970723E   1C                   ENDDO
053400970723     C*
053500970723     C                   ENDSR
053600970723     C*------------------------------------------------------------------------*
053700970723     C* CHKSDO - CONTROLLA VALIDITA' RECORD
053800970723     C*------------------------------------------------------------------------*
053900970723     C     CHKSDO        BEGSR
054000970723     C*
054100970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
054200970723     C*
054300970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
054400970723IF  1C     SDODAT        IFGE      PARDAT
054500970723     C     SDODAT        ANDLE     PARDA2
054600970723X   1C                   ELSE
054700970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
054800970723E   1C                   ENDIF
054900970723     C*
055000970723     C                   ENDSR
055100970723     C*------------------------------------------------------------------------*
055200970723     C* AGGSDO - SCRIVE / CANCELLA RECORD
055300970723     C*------------------------------------------------------------------------*
055400970723     C     AGGSDO        BEGSR
055500970723     C*
055600970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
055700990609     C                   WRITE     SISDOS                               98
055800970723     C*
055900970723     C* NO ERRORE, CANCELLA RECORD
056000970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
056100990609     C                   DELETE    SISDO000
056200990609     C                   ADD       1             TOTSDO                         *TOT AGGIORNATI SISD
056300970723     C*
056400970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
056500970723X   1C                   ELSE                                                   *ERRORE
056600970723     C                   MOVEL     'O'           WSDX
056700970723     C                   EXSR      STAERR
056800970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
056900970723E   1C                   ENDIF
057000970723     C*
057100970723     C                   ENDSR
057200970723     C*------------------------------------------------------------------------*
057300990609     C* ELASDP - ELABORAZIONE PER SISDP
057400970723     C*------------------------------------------------------------------------*
057500970723     C     ELASDP        BEGSR
057600970723     C*
057700970723     C* POSIZIONAMENTO E PRIMA LETTURA
057800970723     C                   EXSR      SETSDP
057900970723     C*
058000970723     C* CICLO FINO A FINE CICLO O FINE FILE
058100970723DO  1C     $FINE         DOWEQ     'N'
058200970723     C     $FSDP         ANDEQ     'N'
058300970723     C*
058400970723     C* SCRIVE / CANCELLA RECORD
058500970723     C                   EXSR      AGGSDP
058600970723     C*
058700970723     C* LETTURA SUCESSIVA
058800970723     C                   EXSR      LETSDP
058900970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
059000970723E   1C                   ENDDO                                                  *FINE FILE
059100970723     C*
059200970723     C                   MOVEL     'P'           WSDX
059300970723     C                   EXSR      STATOT
059400970723     C*
059500970723     C                   ENDSR
059600970723     C*------------------------------------------------------------------------*
059700970723     C* SETSDP - POSIZIONAMENTO E PRIMA LETTURA
059800970723     C*------------------------------------------------------------------------*
059900970723     C     SETSDP        BEGSR
060000970723     C*
060100970723     C* REIMPOSTA FINE PGM
060200970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
060300970723     C*
060400990609     C     *LOVAL        SETLL     SISDP01L                           99
060500970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
060600970723     C                   MOVEL     'S'           $FSDP                          *FINE CICLO
060700970723X   1C                   ELSE
060800970723     C                   EXSR      LETSDP                                       *LETTURA RECORD
060900970723E   1C                   ENDIF
061000970723     C*
061100970723     C                   ENDSR
061200970723     C*------------------------------------------------------------------------*
061300970723     C* LETSDP - LETTURA PROSSIMO RECORD
061400970723     C*------------------------------------------------------------------------*
061500970723     C     LETSDP        BEGSR
061600970723     C*
061700970723     C* LETTURA FINO A:
061800970723     C*  - FINE FILE
061900970723     C*  - TROVATO RECORD VALIDO
062000970723     C                   MOVEL     'N'           WRECOK
062100970723DO  1C     $FSDP         DOWEQ     'N'
062200970723     C     WRECOK        ANDEQ     'N'
062300990609     C                   READ      SISDP01L                               99
062400970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
062500970723     C                   MOVEL     'S'           $FSDP                          *FINE CICLO
062600970723X   2C                   ELSE
062700970723     C                   EXSR      CHKSDP                                       *CONTROLLA RECORD
062800970723E   2C                   ENDIF
062900970723E   1C                   ENDDO
063000970723     C*
063100970723     C                   ENDSR
063200970723     C*------------------------------------------------------------------------*
063300970723     C* CHKSDP - CONTROLLA VALIDITA' RECORD
063400970723     C*------------------------------------------------------------------------*
063500970723     C     CHKSDP        BEGSR
063600970723     C*
063700970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
063800970723     C*
063900970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
064000970723IF  1C     SDPDAT        IFGE      PARDAT
064100970723     C     SDPDAT        ANDLE     PARDA2
064200970723X   1C                   ELSE
064300970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
064400970723E   1C                   ENDIF
064500970723     C*
064600970723     C                   ENDSR
064700970723     C*------------------------------------------------------------------------*
064800970723     C* AGGSDP - SCRIVE / CANCELLA RECORD
064900970723     C*------------------------------------------------------------------------*
065000970723     C     AGGSDP        BEGSR
065100970723     C*
065200970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
065300990609     C                   WRITE     SISDPS                               98
065400970723     C*
065500970723     C* NO ERRORE, CANCELLA RECORD
065600970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
065700990609     C                   DELETE    SISDP000
065800990609     C                   ADD       1             TOTSDP                         *TOT AGGIORNATI SISD
065900970723     C*
066000970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
066100970723X   1C                   ELSE                                                   *ERRORE
066200970723     C                   MOVEL     'P'           WSDX
066300970723     C                   EXSR      STAERR
066400970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
066500970723E   1C                   ENDIF
066600970723     C*
066700970723     C                   ENDSR
066800970723     C*------------------------------------------------------------------------*
066900990609     C* ELASDR - ELABORAZIONE PER SISDR
067000970723     C*------------------------------------------------------------------------*
067100970723     C     ELASDR        BEGSR
067200970723     C*
067300970723     C* POSIZIONAMENTO E PRIMA LETTURA
067400970723     C                   EXSR      SETSDR
067500970723     C*
067600970723     C* CICLO FINO A FINE CICLO O FINE FILE
067700970723DO  1C     $FINE         DOWEQ     'N'
067800970723     C     $FSDR         ANDEQ     'N'
067900970723     C*
068000970723     C* SCRIVE / CANCELLA RECORD
068100970723     C                   EXSR      AGGSDR
068200970723     C*
068300970723     C* LETTURA SUCESSIVA
068400970723     C                   EXSR      LETSDR
068500970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
068600970723E   1C                   ENDDO                                                  *FINE FILE
068700970723     C*
068800970723     C                   MOVEL     'R'           WSDX
068900970723     C                   EXSR      STATOT
069000970723     C*
069100970723     C                   ENDSR
069200970723     C*------------------------------------------------------------------------*
069300970723     C* SETSDR - POSIZIONAMENTO E PRIMA LETTURA
069400970723     C*------------------------------------------------------------------------*
069500970723     C     SETSDR        BEGSR
069600970723     C*
069700970723     C* REIMPOSTA FINE PGM
069800970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
069900970723     C*
070000990609     C     *LOVAL        SETLL     SISDR01L                           99
070100970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
070200970723     C                   MOVEL     'S'           $FSDR                          *FINE CICLO
070300970723X   1C                   ELSE
070400970723     C                   EXSR      LETSDR                                       *LETTURA RECORD
070500970723E   1C                   ENDIF
070600970723     C*
070700970723     C                   ENDSR
070800970723     C*------------------------------------------------------------------------*
070900970723     C* LETSDR - LETTURA PROSSIMO RECORD
071000970723     C*------------------------------------------------------------------------*
071100970723     C     LETSDR        BEGSR
071200970723     C*
071300970723     C* LETTURA FINO A:
071400970723     C*  - FINE FILE
071500970723     C*  - TROVATO RECORD VALIDO
071600970723     C                   MOVEL     'N'           WRECOK
071700970723DO  1C     $FSDR         DOWEQ     'N'
071800970723     C     WRECOK        ANDEQ     'N'
071900990609     C                   READ      SISDR01L                               99
072000970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
072100970723     C                   MOVEL     'S'           $FSDR                          *FINE CICLO
072200970723X   2C                   ELSE
072300970723     C                   EXSR      CHKSDR                                       *CONTROLLA RECORD
072400970723E   2C                   ENDIF
072500970723E   1C                   ENDDO
072600970723     C*
072700970723     C                   ENDSR
072800970723     C*------------------------------------------------------------------------*
072900970723     C* CHKSDR - CONTROLLA VALIDITA' RECORD
073000970723     C*------------------------------------------------------------------------*
073100970723     C     CHKSDR        BEGSR
073200970723     C*
073300970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
073400970723     C*
073500970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
073600970723IF  1C     SDRDAT        IFGE      PARDAT
073700970723     C     SDRDAT        ANDLE     PARDA2
073800970723X   1C                   ELSE
073900970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
074000970723E   1C                   ENDIF
074100970723     C*
074200970723     C                   ENDSR
074300970723     C*------------------------------------------------------------------------*
074400970723     C* AGGSDR - SCRIVE / CANCELLA RECORD
074500970723     C*------------------------------------------------------------------------*
074600970723     C     AGGSDR        BEGSR
074700970723     C*
074800970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
074900990609     C                   WRITE     SISDRS                               98
075000970723     C*
075100970723     C* NO ERRORE, CANCELLA RECORD
075200970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
075300990609     C                   DELETE    SISDR000
075400990609     C                   ADD       1             TOTSDR                         *TOT AGGIORNATI SISD
075500970723     C*
075600970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
075700970723X   1C                   ELSE                                                   *ERRORE
075800970723     C                   MOVEL     'R'           WSDX
075900970723     C                   EXSR      STAERR
076000970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
076100970723E   1C                   ENDIF
076200970723     C*
076300970723     C                   ENDSR
076400970723     C*------------------------------------------------------------------------*
076500990609     C* ELASDT - ELABORAZIONE PER SISDT
076600970723     C*------------------------------------------------------------------------*
076700970723     C     ELASDT        BEGSR
076800970723     C*
076900970723     C* POSIZIONAMENTO E PRIMA LETTURA
077000970723     C                   EXSR      SETSDT
077100970723     C*
077200970723     C* CICLO FINO A FINE CICLO O FINE FILE
077300970723DO  1C     $FINE         DOWEQ     'N'
077400970723     C     $FSDT         ANDEQ     'N'
077500970723     C*
077600970723     C* SCRIVE / CANCELLA RECORD
077700970723     C                   EXSR      AGGSDT
077800970723     C*
077900970723     C* LETTURA SUCESSIVA
078000970723     C                   EXSR      LETSDT
078100970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
078200970723E   1C                   ENDDO                                                  *FINE FILE
078300970723     C*
078400970723     C                   MOVEL     'T'           WSDX
078500970723     C                   EXSR      STATOT
078600970723     C*
078700970723     C                   ENDSR
078800970723     C*------------------------------------------------------------------------*
078900970723     C* SETSDT - POSIZIONAMENTO E PRIMA LETTURA
079000970723     C*------------------------------------------------------------------------*
079100970723     C     SETSDT        BEGSR
079200970723     C*
079300970723     C* REIMPOSTA FINE PGM
079400970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
079500970723     C*
079600990609     C     *LOVAL        SETLL     SISDT01L                           99
079700970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
079800970723     C                   MOVEL     'S'           $FSDT                          *FINE CICLO
079900970723X   1C                   ELSE
080000970723     C                   EXSR      LETSDT                                       *LETTURA RECORD
080100970723E   1C                   ENDIF
080200970723     C*
080300970723     C                   ENDSR
080400970723     C*------------------------------------------------------------------------*
080500970723     C* LETSDT - LETTURA PROSSIMO RECORD
080600970723     C*------------------------------------------------------------------------*
080700970723     C     LETSDT        BEGSR
080800970723     C*
080900970723     C* LETTURA FINO A:
081000970723     C*  - FINE FILE
081100970723     C*  - TROVATO RECORD VALIDO
081200970723     C                   MOVEL     'N'           WRECOK
081300970723DO  1C     $FSDT         DOWEQ     'N'
081400970723     C     WRECOK        ANDEQ     'N'
081500990609     C                   READ      SISDT01L                               99
081600970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
081700970723     C                   MOVEL     'S'           $FSDT                          *FINE CICLO
081800970723X   2C                   ELSE
081900970723     C                   EXSR      CHKSDT                                       *CONTROLLA RECORD
082000970723E   2C                   ENDIF
082100970723E   1C                   ENDDO
082200970723     C*
082300970723     C                   ENDSR
082400970723     C*------------------------------------------------------------------------*
082500970723     C* CHKSDT - CONTROLLA VALIDITA' RECORD
082600970723     C*------------------------------------------------------------------------*
082700970723     C     CHKSDT        BEGSR
082800970723     C*
082900970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
083000970723     C*
083100970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
083200970723IF  1C     SDTDAT        IFGE      PARDAT
083300970723     C     SDTDAT        ANDLE     PARDA2
083400970723X   1C                   ELSE
083500970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083600970723E   1C                   ENDIF
083700970723     C*
083800970723     C                   ENDSR
083900970723     C*------------------------------------------------------------------------*
084000970723     C* AGGSDT - SCRIVE / CANCELLA RECORD
084100970723     C*------------------------------------------------------------------------*
084200970723     C     AGGSDT        BEGSR
084300970723     C*
084400970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
084500990609     C                   WRITE     SISDTS                               98
084600970723     C*
084700970723     C* NO ERRORE, CANCELLA RECORD
084800970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
084900990609     C                   DELETE    SISDT000
085000990609     C                   ADD       1             TOTSDT                         *TOT AGGIORNATI SISD
085100970723     C*
085200970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
085300970723X   1C                   ELSE                                                   *ERRORE
085400970723     C                   MOVEL     'T'           WSDX
085500970723     C                   EXSR      STAERR
085600970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
085700970723E   1C                   ENDIF
085800970723     C*
085900970723     C                   ENDSR
086000040312     C*------------------------------------------------------------------------*
086100040312     C* ELAsde - ELABORAZIONE PER SIsde
086200040312     C*------------------------------------------------------------------------*
086300040312     C     ELAsde        BEGSR
086400040312     C*
086500040312     C* POSIZIONAMENTO E PRIMA LETTURA
086600040312     C                   EXSR      SETsde
086700040312     C*
086800040312     C* CICLO FINO A FINE CICLO O FINE FILE
086900040312DO  1C     $FINE         DOWEQ     'N'
087000040312     C     $Fsde         ANDEQ     'N'
087100040312     C*
087200040312     C* SCRIVE / CANCELLA RECORD
087300040312     C                   EXSR      AGGsde
087400040312     C*
087500040312     C* LETTURA SUCESSIVA
087600040312     C                   EXSR      LETsde
087700040312     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
087800040312E   1C                   ENDDO                                                  *FINE FILE
087900040312     C*
088000040323     C                   MOVEL     'E'           WSDX
088100040312     C                   EXSR      STATOT
088200040312     C*
088300040312     C                   ENDSR
088400040312     C*------------------------------------------------------------------------*
088500040312     C* SETsde - POSIZIONAMENTO E PRIMA LETTURA
088600040312     C*------------------------------------------------------------------------*
088700040312     C     SETsde        BEGSR
088800040312     C*
088900040312     C* REIMPOSTA FINE PGM
089000040312     C                   MOVEL     'N'           $FINE                          *FINE PGM
089100040312     C*
089200040312     C     *LOVAL        SETLL     SIsde02L                           99
089300040312IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
089400040312     C                   MOVEL     'S'           $Fsde                          *FINE CICLO
089500040312X   1C                   ELSE
089600040312     C                   EXSR      LETsde                                       *LETTURA RECORD
089700040312E   1C                   ENDIF
089800040312     C*
089900040312     C                   ENDSR
090000040312     C*------------------------------------------------------------------------*
090100040312     C* LETsde - LETTURA PROSSIMO RECORD
090200040312     C*------------------------------------------------------------------------*
090300040312     C     LETsde        BEGSR
090400040312     C*
090500040312     C* LETTURA FINO A:
090600040312     C*  - FINE FILE
090700040312     C*  - TROVATO RECORD VALIDO
090800040312     C                   MOVEL     'N'           WRECOK
090900040312DO  1C     $Fsde         DOWEQ     'N'
091000040312     C     WRECOK        ANDEQ     'N'
091100040312     C                   READ      SIsde02L                               99
091200040312IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
091300040312     C                   MOVEL     'S'           $Fsde                          *FINE CICLO
091400040312X   2C                   ELSE
091500040312     C                   EXSR      CHKsde                                       *CONTROLLA RECORD
091600040312E   2C                   ENDIF
091700040312E   1C                   ENDDO
091800040312     C*
091900040312     C                   ENDSR
092000040312     C*------------------------------------------------------------------------*
092100040312     C* CHKsde - CONTROLLA VALIDITA' RECORD
092200040312     C*------------------------------------------------------------------------*
092300040312     C     CHKsde        BEGSR
092400040312     C*
092500040312     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
092600040312     C*
092700040312     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
092800040312IF  1C     sdeDAT        IFGE      PARDAT
092900040312     C     sdeDAT        ANDLE     PARDA2
093000040312X   1C                   ELSE
093100040312     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
093200040312E   1C                   ENDIF
093300040312     C*
093400040312     C                   ENDSR
093500040312     C*------------------------------------------------------------------------*
093600040312     C* AGGsde - SCRIVE / CANCELLA RECORD
093700040312     C*------------------------------------------------------------------------*
093800040312     C     AGGsde        BEGSR
093900040312     C*
094000040312     C* SCRIVE IL RECORD SUL FILE DA SALVARE
094100040312     C                   WRITE     SIsdeS                               98
094200040312     C*
094300040312     C* NO ERRORE, CANCELLA RECORD
094400040312IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
094500040312     C                   DELETE    SIsde000
094600040312     C                   ADD       1             TOTsde                         *TOT AGGIORNATI SISD
094700040312     C*
094800040312     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
094900040312X   1C                   ELSE                                                   *ERRORE
095000040323     C                   MOVEL     'E'           WSDX
095100040312     C                   EXSR      STAERR
095200040312     C                   MOVEL     'S'           $FINE                          *FINE CICLO
095300040312E   1C                   ENDIF
095400040312     C*
095500040312     C                   ENDSR
095600050124     C*------------------------------------------------------------------------*
095700050124     C* ELApes - ELABORAZIONE PER SIpes
095800050124     C*------------------------------------------------------------------------*
095900050124     C     ELApes        BEGSR
096000050124     C*
096100050124     C* POSIZIONAMENTO E PRIMA LETTURA
096200050124     C                   EXSR      SETpes
096300050124     C*
096400050124     C* CICLO FINO A FINE CICLO O FINE FILE
096500050124DO  1C     $FINE         DOWEQ     'N'
096600050124     C     $Fpes         ANDEQ     'N'
096700050124     C*
096800050124     C* SCRIVE / CANCELLA RECORD
096900050124     C                   EXSR      AGGpes
097000050124     C*
097100050124     C* LETTURA SUCESSIVA
097200050124     C                   EXSR      LETpes
097300050124     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
097400050124E   1C                   ENDDO                                                  *FINE FILE
097500050124     C*
097600050124     C                   MOVEL     'W'           WSDX                           *Weight
097700050124     C                   EXSR      STATOT
097800050124     C*
097900050124     C                   ENDSR
098000050124     C*------------------------------------------------------------------------*
098100050124     C* SETpes - POSIZIONAMENTO E PRIMA LETTURA
098200050124     C*------------------------------------------------------------------------*
098300050124     C     SETpes        BEGSR
098400050124     C*
098500050124     C* REIMPOSTA FINE PGM
098600050124     C                   MOVEL     'N'           $FINE                          *FINE PGM
098700050124     C*
098800050124     C     *LOVAL        SETLL     SIpes01L                           99
098900050124IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
099000050124     C                   MOVEL     'S'           $Fpes                          *FINE CICLO
099100050124X   1C                   ELSE
099200050124     C                   EXSR      LETpes                                       *LETTURA RECORD
099300050124E   1C                   ENDIF
099400050124     C*
099500050124     C                   ENDSR
099600050124     C*------------------------------------------------------------------------*
099700050124     C* LETpes - LETTURA PROSSIMO RECORD
099800050124     C*------------------------------------------------------------------------*
099900050124     C     LETpes        BEGSR
100000050124     C*
100100050124     C* LETTURA FINO A:
100200050124     C*  - FINE FILE
100300050124     C*  - TROVATO RECORD VALIDO
100400050124     C                   MOVEL     'N'           WRECOK
100500050124DO  1C     $Fpes         DOWEQ     'N'
100600050124     C     WRECOK        ANDEQ     'N'
100700050124     C                   READ      SIpes01L                               99
100800050124IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
100900050124     C                   MOVEL     'S'           $Fpes                          *FINE CICLO
101000050124X   2C                   ELSE
101100050124     C                   EXSR      CHKpes                                       *CONTROLLA RECORD
101200050124E   2C                   ENDIF
101300050124E   1C                   ENDDO
101400050124     C*
101500050124     C                   ENDSR
101600050124     C*------------------------------------------------------------------------*
101700050124     C* CHKpes - CONTROLLA VALIDITA' RECORD
101800050124     C*------------------------------------------------------------------------*
101900050124     C     CHKpes        BEGSR
102000050124     C*
102100050124     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
102200050124     C*
102300050124     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
102400050124IF  1C     pesDAT        IFGE      PARDAT
102500050124     C     pesDAT        ANDLE     PARDA2
102600050124X   1C                   ELSE
102700050124     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
102800050124E   1C                   ENDIF
102900050124     C*
103000050124     C                   ENDSR
103100050124     C*------------------------------------------------------------------------*
103200050124     C* AGGpes - SCRIVE / CANCELLA RECORD
103300050124     C*------------------------------------------------------------------------*
103400050124     C     AGGpes        BEGSR
103500050124     C*
103600050124     C* SCRIVE IL RECORD SUL FILE DA SALVARE
103700050124     C                   WRITE     SIpesS                               98
103800050124     C*
103900050124     C* NO ERRORE, CANCELLA RECORD
104000050124IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
104100050124     C                   DELETE    SIpes000
104200050124     C                   ADD       1             TOTpes                         *TOT AGGIORNATI SISD
104300050124     C*
104400050124     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
104500050124X   1C                   ELSE                                                   *ERRORE
104600050124     C                   MOVEL     'W'           WSDX
104700050124     C                   EXSR      STAERR
104800050124     C                   MOVEL     'S'           $FINE                          *FINE CICLO
104900050124E   1C                   ENDIF
105000050124     C*
105100050124     C                   ENDSR
105200050124     C*------------------------------------------------------------------------*
105300050124     C* ELAvlm - ELABORAZIONE PER SIvlm
105400050124     C*------------------------------------------------------------------------*
105500050124     C     ELAvlm        BEGSR
105600050124     C*
105700050124     C* POSIZIONAMENTO E PRIMA LETTURA
105800050124     C                   EXSR      SETvlm
105900050124     C*
106000050124     C* CICLO FINO A FINE CICLO O FINE FILE
106100050124DO  1C     $FINE         DOWEQ     'N'
106200050124     C     $Fvlm         ANDEQ     'N'
106300050124     C*
106400050124     C* SCRIVE / CANCELLA RECORD
106500050124     C                   EXSR      AGGvlm
106600050124     C*
106700050124     C* LETTURA SUCESSIVA
106800050124     C                   EXSR      LETvlm
106900050124     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
107000050124E   1C                   ENDDO                                                  *FINE FILE
107100050124     C*
107200050124     C                   MOVEL     'V'           WSDX                           *Weight
107300050124     C                   EXSR      STATOT
107400050124     C*
107500050124     C                   ENDSR
107600050124     C*------------------------------------------------------------------------*
107700050124     C* SETvlm - POSIZIONAMENTO E PRIMA LETTURA
107800050124     C*------------------------------------------------------------------------*
107900050124     C     SETvlm        BEGSR
108000050124     C*
108100050124     C* REIMPOSTA FINE PGM
108200050124     C                   MOVEL     'N'           $FINE                          *FINE PGM
108300050124     C*
108400050124     C     *LOVAL        SETLL     SIvlm01L                           99
108500050124IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
108600050124     C                   MOVEL     'S'           $Fvlm                          *FINE CICLO
108700050124X   1C                   ELSE
108800050124     C                   EXSR      LETvlm                                       *LETTURA RECORD
108900050124E   1C                   ENDIF
109000050124     C*
109100050124     C                   ENDSR
109200050124     C*------------------------------------------------------------------------*
109300050124     C* LETvlm - LETTURA PROSSIMO RECORD
109400050124     C*------------------------------------------------------------------------*
109500050124     C     LETvlm        BEGSR
109600050124     C*
109700050124     C* LETTURA FINO A:
109800050124     C*  - FINE FILE
109900050124     C*  - TROVATO RECORD VALIDO
110000050124     C                   MOVEL     'N'           WRECOK
110100050124DO  1C     $Fvlm         DOWEQ     'N'
110200050124     C     WRECOK        ANDEQ     'N'
110300050124     C                   READ      SIvlm01L                               99
110400050124IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
110500050124     C                   MOVEL     'S'           $Fvlm                          *FINE CICLO
110600050124X   2C                   ELSE
110700050124     C                   EXSR      CHKvlm                                       *CONTROLLA RECORD
110800050124E   2C                   ENDIF
110900050124E   1C                   ENDDO
111000050124     C*
111100050124     C                   ENDSR
111200050124     C*------------------------------------------------------------------------*
111300050124     C* CHKvlm - CONTROLLA VALIDITA' RECORD
111400050124     C*------------------------------------------------------------------------*
111500050124     C     CHKvlm        BEGSR
111600050124     C*
111700050124     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
111800050124     C*
111900050124     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
112000050124IF  1C     vlmDAT        IFGE      PARDAT
112100050124     C     vlmDAT        ANDLE     PARDA2
112200050124X   1C                   ELSE
112300050124     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
112400050124E   1C                   ENDIF
112500050124     C*
112600050124     C                   ENDSR
112700050124     C*------------------------------------------------------------------------*
112800050124     C* AGGvlm - SCRIVE / CANCELLA RECORD
112900050124     C*------------------------------------------------------------------------*
113000050124     C     AGGvlm        BEGSR
113100050124     C*
113200050124     C* SCRIVE IL RECORD SUL FILE DA SALVARE
113300050124     C                   WRITE     SIvlmS                               98
113400050124     C*
113500050124     C* NO ERRORE, CANCELLA RECORD
113600050124IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
113700050124     C                   DELETE    SIvol000
113800050124     C                   ADD       1             TOTvlm                         *TOT AGGIORNATI SIvl
113900050124     C*
114000050124     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
114100050124X   1C                   ELSE                                                   *ERRORE
114200050124     C                   MOVEL     'V'           WSDX
114300050124     C                   EXSR      STAERR
114400050124     C                   MOVEL     'S'           $FINE                          *FINE CICLO
114500050124E   1C                   ENDIF
114600050124     C*
114700050124     C                   ENDSR
114800970723     C*------------------------------------------------------------------------*
114900970723     C* STAERR - STAMPA ERRORE
115000970723     C*------------------------------------------------------------------------*
115100970723     C     STAERR        BEGSR
115200970723     C*
115300970723     C* FINE PAGINA, STAMPA TESTATA
115400970723IF  1C     *INOF         IFEQ      *ON
115500970723     C                   EXCEPT    TEST
115600970723     C                   SETOFF                                       OF
115700970723E   1C                   ENDIF
115800970723     C*
115900990609     C* STAMPA ERRORE PER -SISDA-
116000970723IF  1C     WSDX          IFEQ      'A'
116100970723     C                   EXCEPT    ESDA
116200970723E   1C                   ENDIF
116300970723     C*
116400990609     C* STAMPA ERRORE PER -SISDC-
116500970723IF  1C     WSDX          IFEQ      'C'
116600970723     C                   EXCEPT    ESDC
116700970723E   1C                   ENDIF
116800970723     C*
116900990609     C* STAMPA ERRORE PER -SISDF-
117000970723IF  1C     WSDX          IFEQ      'F'
117100970723     C                   EXCEPT    ESDF
117200970723E   1C                   ENDIF
117300970723     C*
117400040312     C* STAMPA ERRORE PER -SISDO-
117500970723IF  1C     WSDX          IFEQ      'O'
117600040312     C                   EXCEPT    ESDO
117700970723E   1C                   ENDIF
117800970723     C*
117900990609     C* STAMPA ERRORE PER -SISDP-
118000970723IF  1C     WSDX          IFEQ      'P'
118100970723     C                   EXCEPT    ESDP
118200970723E   1C                   ENDIF
118300970723     C*
118400990609     C* STAMPA ERRORE PER -SISDR-
118500970723IF  1C     WSDX          IFEQ      'R'
118600970723     C                   EXCEPT    ESDR
118700970723E   1C                   ENDIF
118800970723     C*
118900990609     C* STAMPA ERRORE PER -SISDT-
119000970723IF  1C     WSDX          IFEQ      'T'
119100970723     C                   EXCEPT    ESDT
119200970723E   1C                   ENDIF
119300040312     C* STAMPA ERRORE PER -SISDE-
119400040312IF  1C     WSDX          IFEQ      'E'
119500040312     C                   EXCEPT    ESDE
119600040312E   1C                   ENDIF
119700970723     C*
119800050124     C* STAMPA ERRORE PER -SIpes-
119900050124IF  1C     WSDX          IFEQ      'W'
120000050124     C                   EXCEPT    Epes
120100050124E   1C                   ENDIF
120200050124     C* STAMPA ERRORE PER -SIvlm-
120300050124IF  1C     WSDX          IFEQ      'V'
120400050124     C                   EXCEPT    Evlm
120500050124E   1C                   ENDIF
120600050124     C*
120700970723     C                   ENDSR
120800970723     C*------------------------------------------------------------------------*
120900970723     C* STATOT - STAMPA RIEPILOGO ELABORAZIONE
121000970723     C*------------------------------------------------------------------------*
121100970723     C     STATOT        BEGSR
121200970723     C*
121300970723     C* FINE PAGINA, STAMPA TESTATA
121400970723IF  1C     *INOF         IFEQ      *ON
121500970723     C                   EXCEPT    TEST
121600970723     C                   SETOFF                                       OF
121700970723E   1C                   ENDIF
121800970723     C*
121900990609     C* STAMPA ERRORE PER -SISDA-
122000970723IF  1C     WSDX          IFEQ      'A'
122100970723     C                   EXCEPT    TSDA
122200970723E   1C                   ENDIF
122300970723     C*
122400990609     C* STAMPA ERRORE PER -SISDC-
122500970723IF  1C     WSDX          IFEQ      'C'
122600970723     C                   EXCEPT    TSDC
122700970723E   1C                   ENDIF
122800970723     C*
122900990609     C* STAMPA ERRORE PER -SISDF-
123000970723IF  1C     WSDX          IFEQ      'F'
123100970723     C                   EXCEPT    TSDF
123200970723E   1C                   ENDIF
123300970723     C*
123400990609     C* STAMPA ERRORE PER -SISDO-
123500970723IF  1C     WSDX          IFEQ      'O'
123600970723     C                   EXCEPT    TSDO
123700970723E   1C                   ENDIF
123800970723     C*
123900990609     C* STAMPA ERRORE PER -SISDP-
124000970723IF  1C     WSDX          IFEQ      'P'
124100970723     C                   EXCEPT    TSDP
124200970723E   1C                   ENDIF
124300970723     C*
124400990609     C* STAMPA ERRORE PER -SISDR-
124500970723IF  1C     WSDX          IFEQ      'R'
124600970723     C                   EXCEPT    TSDR
124700970723E   1C                   ENDIF
124800970723     C*
124900990609     C* STAMPA ERRORE PER -SISDT-
125000970723IF  1C     WSDX          IFEQ      'T'
125100970723     C                   EXCEPT    TSDT
125200970723E   1C                   ENDIF
125300040312     C*
125400040312     C* STAMPA ERRORE PER -SISDE-
125500040312IF  1C     WSDX          IFEQ      'E'
125600040312     C                   EXCEPT    TSDE
125700040312E   1C                   ENDIF
125800050124     C*
125900050124     C* STAMPA ERRORE PER -SIpes-
126000050124IF  1C     WSDX          IFEQ      'W'
126100050124     C                   EXCEPT    Tpes
126200050124E   1C                   ENDIF
126300050124     C*
126400050124     C* STAMPA ERRORE PER -SIvlm-
126500050124IF  1C     WSDX          IFEQ      'V'
126600050124     C                   EXCEPT    Tvlm
126700050124E   1C                   ENDIF
126800970723     C*
126900970723     C                   ENDSR
127000970723     C*------------------------------------------------------------------------*
127100970723     C* FINSR - OPERAZIONI FINALI
127200970723     C*------------------------------------------------------------------------*
127300970723     C     FINSR         BEGSR
127400970723     C*
127500970723     C* AGGIORNA LA TABELLE DELLE DATE DI TRASFERIMENTO
127600020920     C                   Z-ADD     1             KTBKUT
127700020701     C                   MOVEL     '5S'          KTBCOD
127800970723     C     KEYTAB        CHAIN     TABEL00F                           99
127900970723IF  1C     *IN99         IFEQ      *OFF
128000020701     C                   MOVEL     TBLUNI        DS5S
128100020701     C                   MOVEL     PARDA2        §5SDCC
128200020701     C                   MOVE      01            §5SDCC
128300020701     C                   MOVEL     DS5S          TBLUNI
128400970723     C                   UPDATE    TABEL
128500970723E   1C                   ENDIF
128600970723     C*
128700970723     C                   ENDSR
128800020920     C*--------------------------------------------------------------------------------------------*
128900020920     C* REPERISCE I DATI UTENTE
129000020920     C*--------------------------------------------------------------------------------------------*
129100020920     C     REPDATIUTE    BEGSR
129200020920     C*
129300020920     C* INIZIALIZZA VARIABILI DI WRK
129400020920     C                   CLEAR                   TIBS34DS
129500020920     C                   CLEAR                   AZUTEDS
129600020920     C                   CLEAR                   DDATIUTE
129700020920     C*
129800020920     C     *DTAARA       DEFINE    §azute        azuteds
129900020920     C     *DTAARA       DEFINE    §datiute      ddatiute
130000020920     C                   IN(E)     *DTAARA
130100020920if  1C                   IF        %Error
130200020920     c                   EVAL      I34Tla = 'L'
130300020920     C                   CALL      'TIBS34R'
130400020920     C                   PARM                    Tibs34Ds
130500020920     C                   IN        *DTAARA
130600020920e   1C                   ENDIF
130700020920     C*
130800020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
130900020920     C                   MOVEL(P)  RSUT          DSFIRS           20
131000020920     C*
131100020920     C                   ENDSR
131200970723     C*------------------------------------------------------------------------*
131300970723     C* *INZSR - OPERAZIONI INIZIALI
131400970723     C*------------------------------------------------------------------------*
131500970723     C     *INZSR        BEGSR
131600970723     C*--------------------
131700970723     C* RICEVIMENTO PARAMETRI
131800970723     C*--------------------
131900970723     C     *ENTRY        PLIST
132000970723     C                   PARM                    KPJBA
132100970723     C                   MOVEL     KPJBU         PARAM
132200970723     C*--------------------
132300970723     C* VARIABILI RIFERITE AL DATABASE
132400970723     C*--------------------
132500970723     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
132600970723     C     *LIKE         DEFINE    TBLCOD        KTBCOD
132700970723     C*--------------------
132800970723     C* CHAIVI DI LETTURA
132900970723     C*--------------------
133000970723     C* LETTURA TABEL00F - COMPLETA
133100970723     C     KEYTAB        KLIST
133200970723     C                   KFLD                    KTBKUT                         *CODICE UTENTE
133300970723     C                   KFLD                    KTBCOD                         *CODICE TABELLA
133400970723     C*--------------------
133500970723     C* VARIABILI DI CONTROLLO
133600970723     C*--------------------
133700970723     C                   MOVEL     'N'           $FINE             1            *FINE PGM
133800970723     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
133900990609     C                   MOVEL     'N'           $FSDA             1            *FINE LETTURA SISDA
134000990609     C                   MOVEL     'N'           $FSDC             1            *FINE LETTURA SISDC
134100990609     C                   MOVEL     'N'           $FSDF             1            *FINE LETTURA SISDF
134200990609     C                   MOVEL     'N'           $FSDO             1            *FINE LETTURA SISDO
134300990609     C                   MOVEL     'N'           $FSDP             1            *FINE LETTURA SISDP
134400990609     C                   MOVEL     'N'           $FSDR             1            *FINE LETTURA SISDR
134500990609     C                   MOVEL     'N'           $FSDT             1            *FINE LETTURA SISDT
134600040312     C                   MOVEL     'N'           $FSDe             1            *FINE LETTURA SISDO
134700050124     C                   MOVEL     'N'           $FPes             1            *FINE LETTURA SIpes
134800050124     C                   MOVEL     'N'           $Fvlm             1            *FINE LETTURA SIvlm
134900970723     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
135000990609     C                   Z-ADD     *ZEROS        TOTSDA            9 0          *TOT AGGIORNATI SISD
135100990609     C                   Z-ADD     *ZEROS        TOTSDC            9 0          *TOT AGGIORNATI SISD
135200990609     C                   Z-ADD     *ZEROS        TOTSDF            9 0          *TOT AGGIORNATI SISD
135300990609     C                   Z-ADD     *ZEROS        TOTSDO            9 0          *TOT AGGIORNATI SISD
135400990609     C                   Z-ADD     *ZEROS        TOTSDP            9 0          *TOT AGGIORNATI SISD
135500990609     C                   Z-ADD     *ZEROS        TOTSDR            9 0          *TOT AGGIORNATI SISD
135600990609     C                   Z-ADD     *ZEROS        TOTSDT            9 0          *TOT AGGIORNATI SISD
135700040312     C                   Z-ADD     *ZEROS        TOTSDE            9 0          *TOT AGGIORNATI SISD
135800050124     C                   Z-ADD     *ZEROS        totpes            9 0          *TOT AGGIORNATI SIpe
135900050124     C                   Z-ADD     *ZEROS        totvlm            9 0          *TOT AGGIORNATI SIvl
136000990609     C                   MOVEL     *BLANKS       WSDX              1            *ERRORE DI SISDxx
136100970908     C                   MOVEL     '1'           *INOF                          *OVERFLOW
136200970723     C*
136300970723     C* REPERISCE I DATI DELL'UTENTE
136400020920     C                   EXSR      REPDATIUTE
136500970723     C*
136600970723     C* REPERIMENTO DATA E ORA CORRENTE
136700970723     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
136800970723     C                   MOVE      WN14          DATCOR            8 0          *DATA (8) IN G/M/AA
136900970723     C                   MOVEL     WN14          ORACOR            6 0          *ORA (6)
137000970723     C*
137100970723     C                   ENDSR
137200970723     O*------------------------------------------------------------------------*
137300970724     OQSYSPRT   E            TEST             02
137400970723     O                       RIG(1)              66
137500970723     O                       RIG(2)             132
137600970723     O                       DSFIRS              20
137700970723     O                       DATCOR             107 '  /  /    '
137800970723     O                       ORACOR             118 '  :  :  '
137900970723     O                       PAGE          Z    132
138000970723     O*------------------------
138100970723     O          E            TEST             03
138200970723     O                       RIG(3)              66
138300970723     O                       RIG(3)             132
138400970723     O*------------------------
138500970723     O          E            ESDA        1
138600970723     O                       RIG(4)              66
138700990526     O                       SDAKSC        Z     23
138800990526     O                       SDAANN        Z   +  1
138900990526     O                       SDAMES        Z   +  1
139000990526     O                       SDADLV        Z   +  1
139100990526     O                       SDATSP            +  1
139200970723     O*------------------------
139300970723     O          E            TSDA        1
139400970723     O                       RIG(5)              66
139500970723     O                       TOTSDA        Z     39
139600970723     O*------------------------
139700970723     O          E            ESDC        1
139800970723     O                       RIG(6)              66
139900970723     O                       SDCKSC        Z     23
140000990526     O                       SDCANN        Z   +  1
140100990526     O                       SDCMES        Z   +  1
140200990526     O                       SDCREP        Z   +  1
140300990526     O                       SDCREA        Z   +  1
140400990526     O                       SDCFPE            +  1
140500990526     O                       SDCTSP            +  1
140600990526     O                       SDCCTR        Z   +  1
140700990526     O                       SDCTPO            +  1
140800990526     O                       SDCRBL            +  1
140900990526     O                       SDCTPR            +  1
141000970723     O*------------------------
141100970723     O          E            TSDC        1
141200970723     O                       RIG(7)              66
141300970723     O                       TOTSDC        Z     39
141400970723     O*------------------------
141500970723     O          E            ESDF        1
141600970723     O                       RIG(8)              66
141700990526     O                       SDFANN        Z     20
141800990526     O                       SDFMES        Z   +  1
141900990526     O                       SDFFIL        Z   +  1
142000970723     O*------------------------
142100970723     O          E            TSDF        1
142200970723     O                       RIG(9)              66
142300970723     O                       TOTSDF        Z     39
142400970723     O*------------------------
142500970723     O          E            ESDO        1
142600970723     O                       RIG(10)             66
142700970723     O                       SDOKSC        Z     23
142800990526     O                       SDOANN        Z   +  1
142900990526     O                       SDOMES        Z   +  1
143000970723     O*------------------------
143100970723     O          E            TSDO        1
143200970723     O                       RIG(11)             66
143300970723     O                       TOTSDO        Z     39
143400970723     O*------------------------
143500970723     O          E            ESDP        1
143600970723     O                       RIG(12)             66
143700970723     O                       SDPKSC        Z     23
143800990526     O                       SDPANN        Z   +  1
143900990526     O                       SDPMES        Z   +  1
144000990526     O                       SDPCTR        Z   +  1
144100990526     O                       SDPSVR            +  1
144200990526     O                       SDPFTC            +  1
144300970723     O*------------------------
144400970723     O          E            TSDP        1
144500970723     O                       RIG(13)             66
144600970723     O                       TOTSDP        Z     39
144700970723     O*------------------------
144800970723     O          E            ESDR        1
144900970723     O                       RIG(14)             66
145000990526     O                       SDRANN        Z     20
145100990526     O                       SDRFLP        Z   +  1
145200990526     O                       SDRFLA        Z   +  1
145300990526     O                       SDRMES        Z   +  1
145400990526     O                       SDRFPE            +  1
145500990526     O                       SDRTSP            +  1
145600990526     O                       SDRTPO            +  1
145700990526     O                       SDRRBL            +  1
145800990526     O                       SDRTPR            +  1
145900970723     O*------------------------
146000970723     O          E            TSDR        1
146100970723     O                       RIG(15)             66
146200970723     O                       TOTSDR        Z     39
146300970723     O*------------------------
146400970723     O          E            ESDT        1
146500970723     O                       RIG(16)             66
146600990526     O                       SDTANN        Z     20
146700990526     O                       SDTTFP        Z   +  1
146800990526     O                       SDTTFA        Z   +  1
146900990526     O                       SDTFPE            +  1
147000990526     O                       SDTMES        Z   +  1
147100990526     O                       SDTDEC            +  1
147200990526     O                       SDTTT1        Z   +  1
147300990526     O                       SDTTT2        Z   +  1
147400040524     O                       SDTTT3        Z   +  1
147500040524     O                       SDTTT4        Z   +  1
147600040524     O                       SDTTT5        Z   +  1
147700970723     O*------------------------
147800970723     O          E            TSDT        1
147900970723     O                       RIG(17)             66
148000970723     O                       TOTSDT        Z     39
148100040312     O*------------------------
148200040312     O          E            ESDE        1
148300040312     O                       RIG(18)             66
148400040312     O                       SDeKSC        Z     23
148500040312     O                       SDeANN        Z   +  1
148600040312     O                       SDeMES        Z   +  1
148700040312     O*------------------------
148800040312     O          E            TSDe        1
148900040312     O                       RIG(19)             66
149000040312     O                       TOTSDe        Z     39
149100050124     O*------------------------
149200050124     O          E            EPES        1
149300050124     O                       RIG(21)             66
149400050124     O                       pesKSC        Z     23
149500050124     O                       pesANN        Z   +  1
149600050124     O                       pesMES        Z   +  1
149700050124     O*------------------------
149800050124     O          E            TPES        1
149900050124     O                       RIG(22)             66
150000050124     O                       TOTpes        Z     39
150100050124     O*------------------------
150200050124     O          E            EVLM        1
150300050124     O                       RIG(23)             66
150400050124     O                       vlmKSC        Z     23
150500050124     O                       vlmANN        Z   +  1
150600050124     O                       vlmMES        Z   +  1
150700050124     O*------------------------
150800050124     O          E            TVLM        1
150900050124     O                       RIG(24)             66
151000050124     O                       TOTvlm        Z     39
151100970723     O*------------------------------------------------------------------------*
151200970723** RIG - RIGHE DI STAMPA
151300970723XXXXXXXXXXXXXXXXXXXX            TNSS42R               *** CANCELLA1
151400970724ZIONE SALDI DI SEDE ***        99/99/9999   66:66:66      PAG.99992
151500970723------------------------------------------------------------------3
151600990609ERRORE SISDA -> 9999999 9999 99 999 X                             4
151700990609RECORD AGGIORNATI IN SISDA -> 999999999                           5
151800990609ERRORE SISDC -> 9999999 9999 99 999 999 XX X 999 X X X            6
151900990609RECORD AGGIORNATI IN SISDC -> 999999999                           7
152000990609ERRORE SISDF -> 9999 99 999                                       8
152100990609RECORD AGGIORNATI IN SISDF -> 999999999                           9
152200990609ERRORE SISDO -> 9999999 9999 99                                   10
152300990609RECORD AGGIORNATI IN SISDO -> 999999999                           11
152400990609ERRORE SISDP -> 9999999 9999 99 999 X X                           12
152500990609RECORD AGGIORNATI IN SISDP -> 999999999                           13
152600990609ERRORE SISDR -> 9999 999 999 99 XX X X X X                        14
152700990609RECORD AGGIORNATI IN SISDR -> 999999999                           15
152800990609ERRORE SISDT -> 9999 999 999 XX 99 X 999 999                      16
152900990609RECORD AGGIORNATI IN SISDT -> 999999999                           17
153000040312ERRORE SISDE -> 9999999 9999 99                                   18
153100040312RECORD AGGIORNATI IN SISDE -> 999999999                           19
15320097072320
153300050124ERRORE SIPES -> 9999999 9999 99                                   21
153400050124RECORD AGGIORNATI IN SIPES -> 999999999                           22
153500050124ERRORE SIVLM -> 9999999 9999 99                                   23
153600050124RECORD AGGIORNATI IN SIVLM -> 999999999                           24
