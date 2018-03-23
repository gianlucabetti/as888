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
002100051117     Fsicop01L  UF   E           K DISK
002200970723     F*---
002300970723     F* FILES SALVATI -SALxx-
002400970723     F*---
002500990609     FSISDA00F  O  A E           K DISK
002600990609     F                                     RENAME(SISDA000:SISDAS)
002700990609     FSISDC00F  O  A E           K DISK
002800990609     F                                     RENAME(SISDC000:SISDCS)
002900990609     FSISDF00F  O  A E           K DISK
003000990609     F                                     RENAME(SISDF000:SISDFS)
003100990609     FSISDO00F  O  A E           K DISK
003200990609     F                                     RENAME(SISDO000:SISDOS)
003300990609     FSISDP00F  O  A E           K DISK
003400990609     F                                     RENAME(SISDP000:SISDPS)
003500990609     FSISDR00F  O  A E           K DISK
003600990609     F                                     RENAME(SISDR000:SISDRS)
003700990609     FSISDT00F  O  A E           K DISK
003800990609     F                                     RENAME(SISDT000:SISDTS)
003900040312     FSISDE00F  O  A E           K DISK
004000040312     F                                     RENAME(SISDE000:SISDES)
004100050124     Fsipes00F  O  A E           K DISK
004200050124     F                                     RENAME(sipes000:SIpesS)
004300050124     Fsivlm00F  O  A E           K DISK
004400050124     F                                     RENAME(sivol000:SIvlmS)
004500051116     Fsicop00F  O  A E           K DISK
004600051116     F                                     RENAME(sicop000:SIcopS)
004700970723     F*---
004800970723     F* TABELLE
004900970723     F*---
005000970723     FTABEL00F  UF   E           K DISK
005100970724     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
005200970723     D*------------------------------------------------------------------------*
005300970723     D* SCHIERE
005400970723     D*------------------------------------------------------------------------*
005500970723     D*-------------------
005600970723     D* RIGHE DI STAMPA
005700970723     D*-------------------
005800051117     D RIG             S             66    DIM(26) CTDATA PERRCD(1)
005900970723     D*------------------------------------------------------------------------*
006000970723     D* INPUT
006100970723     D*------------------------------------------------------------------------*
006200970723     D*-------------------
006300020920     D* DS REPERIMENTO DATI UTENTE
006400970723     D*-------------------
006500020920     D TIBS34DS      E DS                                                       *Profili utente
006600020920     D DDATIUTE      E DS                                                       *Dati utente
006700020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006800970723     D*-------------------
006900020701     D* DS DATI STATISTICHE
007000970723     D*-------------------
007100020701     D DS5S          E DS
007200970723     D*-------------------
007300990609     D* DS COMPOSIZIONE DATA -SISDA-
007400970723     D*-------------------
007500970723     D                 DS
007600990526     D  SDAANN                 1      4  0
007700990526     D  SDAMES                 5      6  0
007800990526     D  SDADAT                 1      6  0
007900970723     D*-------------------
008000990609     D* DS COMPOSIZIONE DATA -SISDC-
008100970723     D*-------------------
008200970723     D                 DS
008300990526     D  SDCANN                 1      4  0
008400990526     D  SDCMES                 5      6  0
008500990526     D  SDCDAT                 1      6  0
008600970723     D*-------------------
008700990609     D* DS COMPOSIZIONE DATA -SISDF-
008800970723     D*-------------------
008900970723     D                 DS
009000990526     D  SDFANN                 1      4  0
009100990526     D  SDFMES                 5      6  0
009200990526     D  SDFDAT                 1      6  0
009300970723     D*-------------------
009400990609     D* DS COMPOSIZIONE DATA -SISDO-
009500970723     D*-------------------
009600970723     D                 DS
009700990526     D  SDOANN                 1      4  0
009800990526     D  SDOMES                 5      6  0
009900990526     D  SDODAT                 1      6  0
010000970723     D*-------------------
010100990609     D* DS COMPOSIZIONE DATA -SISDP-
010200970723     D*-------------------
010300970723     D                 DS
010400990526     D  SDPANN                 1      4  0
010500990526     D  SDPMES                 5      6  0
010600990526     D  SDPDAT                 1      6  0
010700970723     D*-------------------
010800990609     D* DS COMPOSIZIONE DATA -SISDR-
010900970723     D*-------------------
011000970723     D                 DS
011100990526     D  SDRANN                 1      4  0
011200990526     D  SDRMES                 5      6  0
011300990526     D  SDRDAT                 1      6  0
011400970723     D*-------------------
011500990609     D* DS COMPOSIZIONE DATA -SISDT-
011600970723     D*-------------------
011700970723     D                 DS
011800990526     D  SDTANN                 1      4  0
011900990526     D  SDTMES                 5      6  0
012000990526     D  SDTDAT                 1      6  0
012100040312     D*-------------------
012200040312     D* DS COMPOSIZIONE DATA -SISDE-
012300040312     D*-------------------
012400040312     D                 DS
012500040312     D  SDeANN                 1      4  0
012600040312     D  SDeMES                 5      6  0
012700040312     D  SDeDAT                 1      6  0
012800050124     D*-------------------
012900050124     D* DS COMPOSIZIONE DATA -SIpes-
013000050124     D*-------------------
013100050124     D                 DS
013200050124     D  pesANN                 1      4  0
013300050124     D  pesMES                 5      6  0
013400050124     D  pesDAT                 1      6  0
013500050124     D*-------------------
013600050124     D* DS COMPOSIZIONE DATA -SIvlm-
013700050124     D*-------------------
013800050124     D                 DS
013900050124     D  vlmANN                 1      4  0
014000050124     D  vlmMES                 5      6  0
014100050124     D  vlmDAT                 1      6  0
014200051117     D*-------------------
014300051117     D* DS COMPOSIZIONE DATA -SIcop-
014400051117     D*-------------------
014500051117     D                 DS
014600051117     D  copAAS                 1      4  0
014700051117     D  copMES                 5      6  0
014800051117     D  copAAAAmm              1      6  0
014900970723     D*------------------
015000970723     D* DS PARAMETRI LANCIO
015100970723     D*------------------
015200970723     D PARAM           DS
015300990526     D  PARDAT                 1      6  0
015400990526     D  PARDA2                 7     12  0
015500970723     D*-------------------
015600970723     D* ARCHITETTURA
015700970723     D*-------------------
015800970723     D KPJBA         E DS
015900970723     C*------------------------------------------------------------------------*
016000970723     C* MAIN LINES
016100970723     C*------------------------------------------------------------------------*
016200970723     C*
016300990609     C* ELABORAZIONE PER SISDA
016400970723     C                   EXSR      ELASDA
016500970723     C*
016600990609     C* ELABORAZIONE PER SISDC
016700970723     C                   EXSR      ELASDC
016800970723     C*
016900990609     C* ELABORAZIONE PER SISDF
017000970723     C                   EXSR      ELASDF
017100970723     C*
017200990609     C* ELABORAZIONE PER SISDO
017300970723     C                   EXSR      ELASDO
017400970723     C*
017500990609     C* ELABORAZIONE PER SISDP
017600970723     C                   EXSR      ELASDP
017700970723     C*
017800990609     C* ELABORAZIONE PER SISDR
017900970723     C                   EXSR      ELASDR
018000970723     C*
018100990609     C* ELABORAZIONE PER SISDT
018200970723     C                   EXSR      ELASDT
018300040312     C*
018400040312     C* ELABORAZIONE PER SISDE
018500040312     C                   EXSR      ELASDE
018600970723     C*
018700050124     C* ELABORAZIONE PER SIpes
018800050124     C                   EXSR      ELApes
018900050124     C*
019000050124     C* ELABORAZIONE PER SIvlm
019100050124     C                   EXSR      ELAvlm
019200051117     C*
019300051117     C* ELABORAZIONE PER SIcop
019400051117     C                   EXSR      ELAcop
019500050124     C*
019600970723     C* OPERAZIONI FINALI
019700970723     C                   EXSR      FINSR
019800970723     C*
019900970723     C                   SETON                                        LR
020000970723     C*------------------------------------------------------------------------*
020100990609     C* ELASDA - ELABORAZIONE PER SISDA
020200970723     C*------------------------------------------------------------------------*
020300970723     C     ELASDA        BEGSR
020400970723     C*
020500970723     C* POSIZIONAMENTO E PRIMA LETTURA
020600970723     C                   EXSR      SETSDA
020700970723     C*
020800970723     C* CICLO FINO A FINE CICLO O FINE FILE
020900970723DO  1C     $FINE         DOWEQ     'N'
021000970723     C     $FSDA         ANDEQ     'N'
021100970723     C*
021200970723     C* SCRIVE / CANCELLA RECORD
021300970723     C                   EXSR      AGGSDA
021400970723     C*
021500970723     C* LETTURA SUCESSIVA
021600970723     C                   EXSR      LETSDA
021700970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
021800970723E   1C                   ENDDO                                                  *FINE FILE
021900970723     C*
022000970723     C* STAMPA RIEPILOGO ELABORAZIONE
022100970723     C                   MOVEL     'A'           WSDX
022200970723     C                   EXSR      STATOT
022300970723     C*
022400970723     C                   ENDSR
022500970723     C*------------------------------------------------------------------------*
022600970723     C* SETSDA - POSIZIONAMENTO E PRIMA LETTURA
022700970723     C*------------------------------------------------------------------------*
022800970723     C     SETSDA        BEGSR
022900970723     C*
023000970723     C* REIMPOSTA FINE PGM
023100970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
023200970723     C*
023300990609     C     *LOVAL        SETLL     SISDA01L                           99
023400970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
023500970723     C                   MOVEL     'S'           $FSDA                          *FINE CICLO
023600970723X   1C                   ELSE
023700970723     C                   EXSR      LETSDA                                       *LETTURA RECORD
023800970723E   1C                   ENDIF
023900970723     C*
024000970723     C                   ENDSR
024100970723     C*------------------------------------------------------------------------*
024200970723     C* LETSDA - LETTURA PROSSIMO RECORD
024300970723     C*------------------------------------------------------------------------*
024400970723     C     LETSDA        BEGSR
024500970723     C*
024600970723     C* LETTURA FINO A:
024700970723     C*  - FINE FILE
024800970723     C*  - TROVATO RECORD VALIDO
024900970723     C                   MOVEL     'N'           WRECOK
025000970723DO  1C     $FSDA         DOWEQ     'N'
025100970723     C     WRECOK        ANDEQ     'N'
025200990609     C                   READ      SISDA01L                               99
025300970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
025400970723     C                   MOVEL     'S'           $FSDA                          *FINE CICLO
025500970723X   2C                   ELSE
025600970723     C                   EXSR      CHKSDA                                       *CONTROLLA RECORD
025700970723E   2C                   ENDIF
025800970723E   1C                   ENDDO
025900970723     C*
026000970723     C                   ENDSR
026100970723     C*------------------------------------------------------------------------*
026200970723     C* CHKSDA - CONTROLLA VALIDITA' RECORD
026300970723     C*------------------------------------------------------------------------*
026400970723     C     CHKSDA        BEGSR
026500970723     C*
026600970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
026700970723     C*
026800970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
026900970723IF  1C     SDADAT        IFGE      PARDAT
027000970723     C     SDADAT        ANDLE     PARDA2
027100970723X   1C                   ELSE
027200970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027300970723E   1C                   ENDIF
027400970723     C*
027500970723     C                   ENDSR
027600970723     C*------------------------------------------------------------------------*
027700970723     C* AGGSDA - SCRIVE / CANCELLA RECORD
027800970723     C*------------------------------------------------------------------------*
027900970723     C     AGGSDA        BEGSR
028000970723     C*
028100970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
028200990609     C                   WRITE     SISDAS                               98
028300970723     C*
028400970723     C* NO ERRORE, CANCELLA RECORD
028500970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
028600990609     C                   DELETE    SISDA000
028700990609     C                   ADD       1             TOTSDA                         *TOT AGGIORNATI SISD
028800970723     C*
028900970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
029000970723X   1C                   ELSE                                                   *ERRORE
029100970723     C                   MOVEL     'A'           WSDX
029200970723     C                   EXSR      STAERR
029300970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
029400970723E   1C                   ENDIF
029500970723     C*
029600970723     C                   ENDSR
029700970723     C*------------------------------------------------------------------------*
029800990609     C* ELASDC - ELABORAZIONE PER SISDC
029900970723     C*------------------------------------------------------------------------*
030000970723     C     ELASDC        BEGSR
030100970723     C*
030200970723     C* POSIZIONAMENTO E PRIMA LETTURA
030300970723     C                   EXSR      SETSDC
030400970723     C*
030500970723     C* CICLO FINO A FINE CICLO O FINE FILE
030600970723DO  1C     $FINE         DOWEQ     'N'
030700970723     C     $FSDC         ANDEQ     'N'
030800970723     C*
030900970723     C* SCRIVE / CANCELLA RECORD
031000970723     C                   EXSR      AGGSDC
031100970723     C*
031200970723     C* LETTURA SUCESSIVA
031300970723     C                   EXSR      LETSDC
031400970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
031500970723E   1C                   ENDDO                                                  *FINE FILE
031600970723     C*
031700970723     C                   MOVEL     'C'           WSDX
031800970723     C                   EXSR      STATOT
031900970723     C*
032000970723     C                   ENDSR
032100970723     C*------------------------------------------------------------------------*
032200970723     C* SETSDC - POSIZIONAMENTO E PRIMA LETTURA
032300970723     C*------------------------------------------------------------------------*
032400970723     C     SETSDC        BEGSR
032500970723     C*
032600970723     C* REIMPOSTA FINE PGM
032700970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
032800970723     C*
032900990609     C     *LOVAL        SETLL     SISDC01L                           99
033000970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
033100970723     C                   MOVEL     'S'           $FSDC                          *FINE CICLO
033200970723X   1C                   ELSE
033300970723     C                   EXSR      LETSDC                                       *LETTURA RECORD
033400970723E   1C                   ENDIF
033500970723     C*
033600970723     C                   ENDSR
033700970723     C*------------------------------------------------------------------------*
033800970723     C* LETSDC - LETTURA PROSSIMO RECORD
033900970723     C*------------------------------------------------------------------------*
034000970723     C     LETSDC        BEGSR
034100970723     C*
034200970723     C* LETTURA FINO A:
034300970723     C*  - FINE FILE
034400970723     C*  - TROVATO RECORD VALIDO
034500970723     C                   MOVEL     'N'           WRECOK
034600970723DO  1C     $FSDC         DOWEQ     'N'
034700970723     C     WRECOK        ANDEQ     'N'
034800990609     C                   READ      SISDC01L                               99
034900970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
035000970723     C                   MOVEL     'S'           $FSDC                          *FINE CICLO
035100970723X   2C                   ELSE
035200970723     C                   EXSR      CHKSDC                                       *CONTROLLA RECORD
035300970723E   2C                   ENDIF
035400970723E   1C                   ENDDO
035500970723     C*
035600970723     C                   ENDSR
035700970723     C*------------------------------------------------------------------------*
035800970723     C* CHKSDC - CONTROLLA VALIDITA' RECORD
035900970723     C*------------------------------------------------------------------------*
036000970723     C     CHKSDC        BEGSR
036100970723     C*
036200970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
036300970723     C*
036400970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
036500970723IF  1C     SDCDAT        IFGE      PARDAT
036600970723     C     SDCDAT        ANDLE     PARDA2
036700970723X   1C                   ELSE
036800970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
036900970723E   1C                   ENDIF
037000970723     C*
037100970723     C                   ENDSR
037200970723     C*------------------------------------------------------------------------*
037300970723     C* AGGSDC - SCRIVE / CANCELLA RECORD
037400970723     C*------------------------------------------------------------------------*
037500970723     C     AGGSDC        BEGSR
037600970723     C*
037700970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
037800990609     C                   WRITE     SISDCS                               98
037900970723     C*
038000970723     C* NO ERRORE, CANCELLA RECORD
038100970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
038200990609     C                   DELETE    SISDC000
038300990609     C                   ADD       1             TOTSDC                         *TOT AGGIORNATI SISD
038400970723     C*
038500970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
038600970723X   1C                   ELSE                                                   *ERRORE
038700970723     C                   MOVEL     'C'           WSDX
038800970723     C                   EXSR      STAERR
038900970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
039000970723E   1C                   ENDIF
039100970723     C*
039200970723     C                   ENDSR
039300970723     C*------------------------------------------------------------------------*
039400990609     C* ELASDF - ELABORAZIONE PER SISDF
039500970723     C*------------------------------------------------------------------------*
039600970723     C     ELASDF        BEGSR
039700970723     C*
039800970723     C* POSIZIONAMENTO E PRIMA LETTURA
039900970723     C                   EXSR      SETSDF
040000970723     C*
040100970723     C* CICLO FINO A FINE CICLO O FINE FILE
040200970723DO  1C     $FINE         DOWEQ     'N'
040300970723     C     $FSDF         ANDEQ     'N'
040400970723     C*
040500970723     C* SCRIVE / CANCELLA RECORD
040600970723     C                   EXSR      AGGSDF
040700970723     C*
040800970723     C* LETTURA SUCESSIVA
040900970723     C                   EXSR      LETSDF
041000970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
041100970723E   1C                   ENDDO                                                  *FINE FILE
041200970723     C*
041300970723     C                   MOVEL     'F'           WSDX
041400970723     C                   EXSR      STATOT
041500970723     C*
041600970723     C                   ENDSR
041700970723     C*------------------------------------------------------------------------*
041800970723     C* SETSDF - POSIZIONAMENTO E PRIMA LETTURA
041900970723     C*------------------------------------------------------------------------*
042000970723     C     SETSDF        BEGSR
042100970723     C*
042200970723     C* REIMPOSTA FINE PGM
042300970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
042400970723     C*
042500990609     C     *LOVAL        SETLL     SISDF01L                           99
042600970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
042700970723     C                   MOVEL     'S'           $FSDF                          *FINE CICLO
042800970723X   1C                   ELSE
042900970723     C                   EXSR      LETSDF                                       *LETTURA RECORD
043000970723E   1C                   ENDIF
043100970723     C*
043200970723     C                   ENDSR
043300970723     C*------------------------------------------------------------------------*
043400970723     C* LETSDF - LETTURA PROSSIMO RECORD
043500970723     C*------------------------------------------------------------------------*
043600970723     C     LETSDF        BEGSR
043700970723     C*
043800970723     C* LETTURA FINO A:
043900970723     C*  - FINE FILE
044000970723     C*  - TROVATO RECORD VALIDO
044100970723     C                   MOVEL     'N'           WRECOK
044200970723DO  1C     $FSDF         DOWEQ     'N'
044300970723     C     WRECOK        ANDEQ     'N'
044400990609     C                   READ      SISDF01L                               99
044500970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
044600970723     C                   MOVEL     'S'           $FSDF                          *FINE CICLO
044700970723X   2C                   ELSE
044800970723     C                   EXSR      CHKSDF                                       *CONTROLLA RECORD
044900970723E   2C                   ENDIF
045000970723E   1C                   ENDDO
045100970723     C*
045200970723     C                   ENDSR
045300970723     C*------------------------------------------------------------------------*
045400970723     C* CHKSDF - CONTROLLA VALIDITA' RECORD
045500970723     C*------------------------------------------------------------------------*
045600970723     C     CHKSDF        BEGSR
045700970723     C*
045800970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
045900970723     C*
046000970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
046100970723IF  1C     SDFDAT        IFGE      PARDAT
046200970723     C     SDFDAT        ANDLE     PARDA2
046300970723X   1C                   ELSE
046400970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046500970723E   1C                   ENDIF
046600970723     C*
046700970723     C                   ENDSR
046800970723     C*------------------------------------------------------------------------*
046900970723     C* AGGSDF - SCRIVE / CANCELLA RECORD
047000970723     C*------------------------------------------------------------------------*
047100970723     C     AGGSDF        BEGSR
047200970723     C*
047300970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
047400990609     C                   WRITE     SISDFS                               98
047500970723     C*
047600970723     C* NO ERRORE, CANCELLA RECORD
047700970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
047800990609     C                   DELETE    SISDF000
047900990609     C                   ADD       1             TOTSDF                         *TOT AGGIORNATI SISD
048000970723     C*
048100970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
048200970723X   1C                   ELSE                                                   *ERRORE
048300970723     C                   MOVEL     'F'           WSDX
048400970723     C                   EXSR      STAERR
048500970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
048600970723E   1C                   ENDIF
048700970723     C*
048800970723     C                   ENDSR
048900970723     C*------------------------------------------------------------------------*
049000990609     C* ELASDO - ELABORAZIONE PER SISDO
049100970723     C*------------------------------------------------------------------------*
049200970723     C     ELASDO        BEGSR
049300970723     C*
049400970723     C* POSIZIONAMENTO E PRIMA LETTURA
049500970723     C                   EXSR      SETSDO
049600970723     C*
049700970723     C* CICLO FINO A FINE CICLO O FINE FILE
049800970723DO  1C     $FINE         DOWEQ     'N'
049900970723     C     $FSDO         ANDEQ     'N'
050000970723     C*
050100970723     C* SCRIVE / CANCELLA RECORD
050200970723     C                   EXSR      AGGSDO
050300970723     C*
050400970723     C* LETTURA SUCESSIVA
050500970723     C                   EXSR      LETSDO
050600970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
050700970723E   1C                   ENDDO                                                  *FINE FILE
050800970723     C*
050900970723     C                   MOVEL     'O'           WSDX
051000970723     C                   EXSR      STATOT
051100970723     C*
051200970723     C                   ENDSR
051300970723     C*------------------------------------------------------------------------*
051400970723     C* SETSDO - POSIZIONAMENTO E PRIMA LETTURA
051500970723     C*------------------------------------------------------------------------*
051600970723     C     SETSDO        BEGSR
051700970723     C*
051800970723     C* REIMPOSTA FINE PGM
051900970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
052000970723     C*
052100990609     C     *LOVAL        SETLL     SISDO01L                           99
052200970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
052300970723     C                   MOVEL     'S'           $FSDO                          *FINE CICLO
052400970723X   1C                   ELSE
052500970723     C                   EXSR      LETSDO                                       *LETTURA RECORD
052600970723E   1C                   ENDIF
052700970723     C*
052800970723     C                   ENDSR
052900970723     C*------------------------------------------------------------------------*
053000970723     C* LETSDO - LETTURA PROSSIMO RECORD
053100970723     C*------------------------------------------------------------------------*
053200970723     C     LETSDO        BEGSR
053300970723     C*
053400970723     C* LETTURA FINO A:
053500970723     C*  - FINE FILE
053600970723     C*  - TROVATO RECORD VALIDO
053700970723     C                   MOVEL     'N'           WRECOK
053800970723DO  1C     $FSDO         DOWEQ     'N'
053900970723     C     WRECOK        ANDEQ     'N'
054000990609     C                   READ      SISDO01L                               99
054100970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
054200970723     C                   MOVEL     'S'           $FSDO                          *FINE CICLO
054300970723X   2C                   ELSE
054400970723     C                   EXSR      CHKSDO                                       *CONTROLLA RECORD
054500970723E   2C                   ENDIF
054600970723E   1C                   ENDDO
054700970723     C*
054800970723     C                   ENDSR
054900970723     C*------------------------------------------------------------------------*
055000970723     C* CHKSDO - CONTROLLA VALIDITA' RECORD
055100970723     C*------------------------------------------------------------------------*
055200970723     C     CHKSDO        BEGSR
055300970723     C*
055400970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
055500970723     C*
055600970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
055700970723IF  1C     SDODAT        IFGE      PARDAT
055800970723     C     SDODAT        ANDLE     PARDA2
055900970723X   1C                   ELSE
056000970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056100970723E   1C                   ENDIF
056200970723     C*
056300970723     C                   ENDSR
056400970723     C*------------------------------------------------------------------------*
056500970723     C* AGGSDO - SCRIVE / CANCELLA RECORD
056600970723     C*------------------------------------------------------------------------*
056700970723     C     AGGSDO        BEGSR
056800970723     C*
056900970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
057000990609     C                   WRITE     SISDOS                               98
057100970723     C*
057200970723     C* NO ERRORE, CANCELLA RECORD
057300970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
057400990609     C                   DELETE    SISDO000
057500990609     C                   ADD       1             TOTSDO                         *TOT AGGIORNATI SISD
057600970723     C*
057700970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
057800970723X   1C                   ELSE                                                   *ERRORE
057900970723     C                   MOVEL     'O'           WSDX
058000970723     C                   EXSR      STAERR
058100970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
058200970723E   1C                   ENDIF
058300970723     C*
058400970723     C                   ENDSR
058500970723     C*------------------------------------------------------------------------*
058600990609     C* ELASDP - ELABORAZIONE PER SISDP
058700970723     C*------------------------------------------------------------------------*
058800970723     C     ELASDP        BEGSR
058900970723     C*
059000970723     C* POSIZIONAMENTO E PRIMA LETTURA
059100970723     C                   EXSR      SETSDP
059200970723     C*
059300970723     C* CICLO FINO A FINE CICLO O FINE FILE
059400970723DO  1C     $FINE         DOWEQ     'N'
059500970723     C     $FSDP         ANDEQ     'N'
059600970723     C*
059700970723     C* SCRIVE / CANCELLA RECORD
059800970723     C                   EXSR      AGGSDP
059900970723     C*
060000970723     C* LETTURA SUCESSIVA
060100970723     C                   EXSR      LETSDP
060200970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
060300970723E   1C                   ENDDO                                                  *FINE FILE
060400970723     C*
060500970723     C                   MOVEL     'P'           WSDX
060600970723     C                   EXSR      STATOT
060700970723     C*
060800970723     C                   ENDSR
060900970723     C*------------------------------------------------------------------------*
061000970723     C* SETSDP - POSIZIONAMENTO E PRIMA LETTURA
061100970723     C*------------------------------------------------------------------------*
061200970723     C     SETSDP        BEGSR
061300970723     C*
061400970723     C* REIMPOSTA FINE PGM
061500970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
061600970723     C*
061700990609     C     *LOVAL        SETLL     SISDP01L                           99
061800970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
061900970723     C                   MOVEL     'S'           $FSDP                          *FINE CICLO
062000970723X   1C                   ELSE
062100970723     C                   EXSR      LETSDP                                       *LETTURA RECORD
062200970723E   1C                   ENDIF
062300970723     C*
062400970723     C                   ENDSR
062500970723     C*------------------------------------------------------------------------*
062600970723     C* LETSDP - LETTURA PROSSIMO RECORD
062700970723     C*------------------------------------------------------------------------*
062800970723     C     LETSDP        BEGSR
062900970723     C*
063000970723     C* LETTURA FINO A:
063100970723     C*  - FINE FILE
063200970723     C*  - TROVATO RECORD VALIDO
063300970723     C                   MOVEL     'N'           WRECOK
063400970723DO  1C     $FSDP         DOWEQ     'N'
063500970723     C     WRECOK        ANDEQ     'N'
063600990609     C                   READ      SISDP01L                               99
063700970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
063800970723     C                   MOVEL     'S'           $FSDP                          *FINE CICLO
063900970723X   2C                   ELSE
064000970723     C                   EXSR      CHKSDP                                       *CONTROLLA RECORD
064100970723E   2C                   ENDIF
064200970723E   1C                   ENDDO
064300970723     C*
064400970723     C                   ENDSR
064500970723     C*------------------------------------------------------------------------*
064600970723     C* CHKSDP - CONTROLLA VALIDITA' RECORD
064700970723     C*------------------------------------------------------------------------*
064800970723     C     CHKSDP        BEGSR
064900970723     C*
065000970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
065100970723     C*
065200970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
065300970723IF  1C     SDPDAT        IFGE      PARDAT
065400970723     C     SDPDAT        ANDLE     PARDA2
065500970723X   1C                   ELSE
065600970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
065700970723E   1C                   ENDIF
065800970723     C*
065900970723     C                   ENDSR
066000970723     C*------------------------------------------------------------------------*
066100970723     C* AGGSDP - SCRIVE / CANCELLA RECORD
066200970723     C*------------------------------------------------------------------------*
066300970723     C     AGGSDP        BEGSR
066400970723     C*
066500970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
066600990609     C                   WRITE     SISDPS                               98
066700970723     C*
066800970723     C* NO ERRORE, CANCELLA RECORD
066900970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
067000990609     C                   DELETE    SISDP000
067100990609     C                   ADD       1             TOTSDP                         *TOT AGGIORNATI SISD
067200970723     C*
067300970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
067400970723X   1C                   ELSE                                                   *ERRORE
067500970723     C                   MOVEL     'P'           WSDX
067600970723     C                   EXSR      STAERR
067700970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
067800970723E   1C                   ENDIF
067900970723     C*
068000970723     C                   ENDSR
068100970723     C*------------------------------------------------------------------------*
068200990609     C* ELASDR - ELABORAZIONE PER SISDR
068300970723     C*------------------------------------------------------------------------*
068400970723     C     ELASDR        BEGSR
068500970723     C*
068600970723     C* POSIZIONAMENTO E PRIMA LETTURA
068700970723     C                   EXSR      SETSDR
068800970723     C*
068900970723     C* CICLO FINO A FINE CICLO O FINE FILE
069000970723DO  1C     $FINE         DOWEQ     'N'
069100970723     C     $FSDR         ANDEQ     'N'
069200970723     C*
069300970723     C* SCRIVE / CANCELLA RECORD
069400970723     C                   EXSR      AGGSDR
069500970723     C*
069600970723     C* LETTURA SUCESSIVA
069700970723     C                   EXSR      LETSDR
069800970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
069900970723E   1C                   ENDDO                                                  *FINE FILE
070000970723     C*
070100970723     C                   MOVEL     'R'           WSDX
070200970723     C                   EXSR      STATOT
070300970723     C*
070400970723     C                   ENDSR
070500970723     C*------------------------------------------------------------------------*
070600970723     C* SETSDR - POSIZIONAMENTO E PRIMA LETTURA
070700970723     C*------------------------------------------------------------------------*
070800970723     C     SETSDR        BEGSR
070900970723     C*
071000970723     C* REIMPOSTA FINE PGM
071100970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
071200970723     C*
071300990609     C     *LOVAL        SETLL     SISDR01L                           99
071400970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
071500970723     C                   MOVEL     'S'           $FSDR                          *FINE CICLO
071600970723X   1C                   ELSE
071700970723     C                   EXSR      LETSDR                                       *LETTURA RECORD
071800970723E   1C                   ENDIF
071900970723     C*
072000970723     C                   ENDSR
072100970723     C*------------------------------------------------------------------------*
072200970723     C* LETSDR - LETTURA PROSSIMO RECORD
072300970723     C*------------------------------------------------------------------------*
072400970723     C     LETSDR        BEGSR
072500970723     C*
072600970723     C* LETTURA FINO A:
072700970723     C*  - FINE FILE
072800970723     C*  - TROVATO RECORD VALIDO
072900970723     C                   MOVEL     'N'           WRECOK
073000970723DO  1C     $FSDR         DOWEQ     'N'
073100970723     C     WRECOK        ANDEQ     'N'
073200990609     C                   READ      SISDR01L                               99
073300970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
073400970723     C                   MOVEL     'S'           $FSDR                          *FINE CICLO
073500970723X   2C                   ELSE
073600970723     C                   EXSR      CHKSDR                                       *CONTROLLA RECORD
073700970723E   2C                   ENDIF
073800970723E   1C                   ENDDO
073900970723     C*
074000970723     C                   ENDSR
074100970723     C*------------------------------------------------------------------------*
074200970723     C* CHKSDR - CONTROLLA VALIDITA' RECORD
074300970723     C*------------------------------------------------------------------------*
074400970723     C     CHKSDR        BEGSR
074500970723     C*
074600970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
074700970723     C*
074800970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
074900970723IF  1C     SDRDAT        IFGE      PARDAT
075000970723     C     SDRDAT        ANDLE     PARDA2
075100970723X   1C                   ELSE
075200970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
075300970723E   1C                   ENDIF
075400970723     C*
075500970723     C                   ENDSR
075600970723     C*------------------------------------------------------------------------*
075700970723     C* AGGSDR - SCRIVE / CANCELLA RECORD
075800970723     C*------------------------------------------------------------------------*
075900970723     C     AGGSDR        BEGSR
076000970723     C*
076100970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
076200990609     C                   WRITE     SISDRS                               98
076300970723     C*
076400970723     C* NO ERRORE, CANCELLA RECORD
076500970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
076600990609     C                   DELETE    SISDR000
076700990609     C                   ADD       1             TOTSDR                         *TOT AGGIORNATI SISD
076800970723     C*
076900970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
077000970723X   1C                   ELSE                                                   *ERRORE
077100970723     C                   MOVEL     'R'           WSDX
077200970723     C                   EXSR      STAERR
077300970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
077400970723E   1C                   ENDIF
077500970723     C*
077600970723     C                   ENDSR
077700970723     C*------------------------------------------------------------------------*
077800990609     C* ELASDT - ELABORAZIONE PER SISDT
077900970723     C*------------------------------------------------------------------------*
078000970723     C     ELASDT        BEGSR
078100970723     C*
078200970723     C* POSIZIONAMENTO E PRIMA LETTURA
078300970723     C                   EXSR      SETSDT
078400970723     C*
078500970723     C* CICLO FINO A FINE CICLO O FINE FILE
078600970723DO  1C     $FINE         DOWEQ     'N'
078700970723     C     $FSDT         ANDEQ     'N'
078800970723     C*
078900970723     C* SCRIVE / CANCELLA RECORD
079000970723     C                   EXSR      AGGSDT
079100970723     C*
079200970723     C* LETTURA SUCESSIVA
079300970723     C                   EXSR      LETSDT
079400970723     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
079500970723E   1C                   ENDDO                                                  *FINE FILE
079600970723     C*
079700970723     C                   MOVEL     'T'           WSDX
079800970723     C                   EXSR      STATOT
079900970723     C*
080000970723     C                   ENDSR
080100970723     C*------------------------------------------------------------------------*
080200970723     C* SETSDT - POSIZIONAMENTO E PRIMA LETTURA
080300970723     C*------------------------------------------------------------------------*
080400970723     C     SETSDT        BEGSR
080500970723     C*
080600970723     C* REIMPOSTA FINE PGM
080700970723     C                   MOVEL     'N'           $FINE                          *FINE PGM
080800970723     C*
080900990609     C     *LOVAL        SETLL     SISDT01L                           99
081000970723IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
081100970723     C                   MOVEL     'S'           $FSDT                          *FINE CICLO
081200970723X   1C                   ELSE
081300970723     C                   EXSR      LETSDT                                       *LETTURA RECORD
081400970723E   1C                   ENDIF
081500970723     C*
081600970723     C                   ENDSR
081700970723     C*------------------------------------------------------------------------*
081800970723     C* LETSDT - LETTURA PROSSIMO RECORD
081900970723     C*------------------------------------------------------------------------*
082000970723     C     LETSDT        BEGSR
082100970723     C*
082200970723     C* LETTURA FINO A:
082300970723     C*  - FINE FILE
082400970723     C*  - TROVATO RECORD VALIDO
082500970723     C                   MOVEL     'N'           WRECOK
082600970723DO  1C     $FSDT         DOWEQ     'N'
082700970723     C     WRECOK        ANDEQ     'N'
082800990609     C                   READ      SISDT01L                               99
082900970723IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
083000970723     C                   MOVEL     'S'           $FSDT                          *FINE CICLO
083100970723X   2C                   ELSE
083200970723     C                   EXSR      CHKSDT                                       *CONTROLLA RECORD
083300970723E   2C                   ENDIF
083400970723E   1C                   ENDDO
083500970723     C*
083600970723     C                   ENDSR
083700970723     C*------------------------------------------------------------------------*
083800970723     C* CHKSDT - CONTROLLA VALIDITA' RECORD
083900970723     C*------------------------------------------------------------------------*
084000970723     C     CHKSDT        BEGSR
084100970723     C*
084200970723     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
084300970723     C*
084400970723     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
084500970723IF  1C     SDTDAT        IFGE      PARDAT
084600970723     C     SDTDAT        ANDLE     PARDA2
084700970723X   1C                   ELSE
084800970723     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
084900970723E   1C                   ENDIF
085000970723     C*
085100970723     C                   ENDSR
085200970723     C*------------------------------------------------------------------------*
085300970723     C* AGGSDT - SCRIVE / CANCELLA RECORD
085400970723     C*------------------------------------------------------------------------*
085500970723     C     AGGSDT        BEGSR
085600970723     C*
085700970723     C* SCRIVE IL RECORD SUL FILE DA SALVARE
085800990609     C                   WRITE     SISDTS                               98
085900970723     C*
086000970723     C* NO ERRORE, CANCELLA RECORD
086100970723IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
086200990609     C                   DELETE    SISDT000
086300990609     C                   ADD       1             TOTSDT                         *TOT AGGIORNATI SISD
086400970723     C*
086500970723     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
086600970723X   1C                   ELSE                                                   *ERRORE
086700970723     C                   MOVEL     'T'           WSDX
086800970723     C                   EXSR      STAERR
086900970723     C                   MOVEL     'S'           $FINE                          *FINE CICLO
087000970723E   1C                   ENDIF
087100970723     C*
087200970723     C                   ENDSR
087300040312     C*------------------------------------------------------------------------*
087400040312     C* ELAsde - ELABORAZIONE PER SIsde
087500040312     C*------------------------------------------------------------------------*
087600040312     C     ELAsde        BEGSR
087700040312     C*
087800040312     C* POSIZIONAMENTO E PRIMA LETTURA
087900040312     C                   EXSR      SETsde
088000040312     C*
088100040312     C* CICLO FINO A FINE CICLO O FINE FILE
088200040312DO  1C     $FINE         DOWEQ     'N'
088300040312     C     $Fsde         ANDEQ     'N'
088400040312     C*
088500040312     C* SCRIVE / CANCELLA RECORD
088600040312     C                   EXSR      AGGsde
088700040312     C*
088800040312     C* LETTURA SUCESSIVA
088900040312     C                   EXSR      LETsde
089000040312     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
089100040312E   1C                   ENDDO                                                  *FINE FILE
089200040312     C*
089300040323     C                   MOVEL     'E'           WSDX
089400040312     C                   EXSR      STATOT
089500040312     C*
089600040312     C                   ENDSR
089700040312     C*------------------------------------------------------------------------*
089800040312     C* SETsde - POSIZIONAMENTO E PRIMA LETTURA
089900040312     C*------------------------------------------------------------------------*
090000040312     C     SETsde        BEGSR
090100040312     C*
090200040312     C* REIMPOSTA FINE PGM
090300040312     C                   MOVEL     'N'           $FINE                          *FINE PGM
090400040312     C*
090500040312     C     *LOVAL        SETLL     SIsde02L                           99
090600040312IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
090700040312     C                   MOVEL     'S'           $Fsde                          *FINE CICLO
090800040312X   1C                   ELSE
090900040312     C                   EXSR      LETsde                                       *LETTURA RECORD
091000040312E   1C                   ENDIF
091100040312     C*
091200040312     C                   ENDSR
091300040312     C*------------------------------------------------------------------------*
091400040312     C* LETsde - LETTURA PROSSIMO RECORD
091500040312     C*------------------------------------------------------------------------*
091600040312     C     LETsde        BEGSR
091700040312     C*
091800040312     C* LETTURA FINO A:
091900040312     C*  - FINE FILE
092000040312     C*  - TROVATO RECORD VALIDO
092100040312     C                   MOVEL     'N'           WRECOK
092200040312DO  1C     $Fsde         DOWEQ     'N'
092300040312     C     WRECOK        ANDEQ     'N'
092400040312     C                   READ      SIsde02L                               99
092500040312IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
092600040312     C                   MOVEL     'S'           $Fsde                          *FINE CICLO
092700040312X   2C                   ELSE
092800040312     C                   EXSR      CHKsde                                       *CONTROLLA RECORD
092900040312E   2C                   ENDIF
093000040312E   1C                   ENDDO
093100040312     C*
093200040312     C                   ENDSR
093300040312     C*------------------------------------------------------------------------*
093400040312     C* CHKsde - CONTROLLA VALIDITA' RECORD
093500040312     C*------------------------------------------------------------------------*
093600040312     C     CHKsde        BEGSR
093700040312     C*
093800040312     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
093900040312     C*
094000040312     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
094100040312IF  1C     sdeDAT        IFGE      PARDAT
094200040312     C     sdeDAT        ANDLE     PARDA2
094300040312X   1C                   ELSE
094400040312     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
094500040312E   1C                   ENDIF
094600040312     C*
094700040312     C                   ENDSR
094800040312     C*------------------------------------------------------------------------*
094900040312     C* AGGsde - SCRIVE / CANCELLA RECORD
095000040312     C*------------------------------------------------------------------------*
095100040312     C     AGGsde        BEGSR
095200040312     C*
095300040312     C* SCRIVE IL RECORD SUL FILE DA SALVARE
095400040312     C                   WRITE     SIsdeS                               98
095500040312     C*
095600040312     C* NO ERRORE, CANCELLA RECORD
095700040312IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
095800040312     C                   DELETE    SIsde000
095900040312     C                   ADD       1             TOTsde                         *TOT AGGIORNATI SISD
096000040312     C*
096100040312     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
096200040312X   1C                   ELSE                                                   *ERRORE
096300040323     C                   MOVEL     'E'           WSDX
096400040312     C                   EXSR      STAERR
096500040312     C                   MOVEL     'S'           $FINE                          *FINE CICLO
096600040312E   1C                   ENDIF
096700040312     C*
096800040312     C                   ENDSR
096900050124     C*------------------------------------------------------------------------*
097000050124     C* ELApes - ELABORAZIONE PER SIpes
097100050124     C*------------------------------------------------------------------------*
097200050124     C     ELApes        BEGSR
097300050124     C*
097400050124     C* POSIZIONAMENTO E PRIMA LETTURA
097500050124     C                   EXSR      SETpes
097600050124     C*
097700050124     C* CICLO FINO A FINE CICLO O FINE FILE
097800050124DO  1C     $FINE         DOWEQ     'N'
097900050124     C     $Fpes         ANDEQ     'N'
098000050124     C*
098100050124     C* SCRIVE / CANCELLA RECORD
098200050124     C                   EXSR      AGGpes
098300050124     C*
098400050124     C* LETTURA SUCESSIVA
098500050124     C                   EXSR      LETpes
098600050124     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
098700050124E   1C                   ENDDO                                                  *FINE FILE
098800050124     C*
098900050124     C                   MOVEL     'W'           WSDX                           *Weight
099000050124     C                   EXSR      STATOT
099100050124     C*
099200050124     C                   ENDSR
099300050124     C*------------------------------------------------------------------------*
099400050124     C* SETpes - POSIZIONAMENTO E PRIMA LETTURA
099500050124     C*------------------------------------------------------------------------*
099600050124     C     SETpes        BEGSR
099700050124     C*
099800050124     C* REIMPOSTA FINE PGM
099900050124     C                   MOVEL     'N'           $FINE                          *FINE PGM
100000050124     C*
100100050124     C     *LOVAL        SETLL     SIpes01L                           99
100200050124IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
100300050124     C                   MOVEL     'S'           $Fpes                          *FINE CICLO
100400050124X   1C                   ELSE
100500050124     C                   EXSR      LETpes                                       *LETTURA RECORD
100600050124E   1C                   ENDIF
100700050124     C*
100800050124     C                   ENDSR
100900050124     C*------------------------------------------------------------------------*
101000050124     C* LETpes - LETTURA PROSSIMO RECORD
101100050124     C*------------------------------------------------------------------------*
101200050124     C     LETpes        BEGSR
101300050124     C*
101400050124     C* LETTURA FINO A:
101500050124     C*  - FINE FILE
101600050124     C*  - TROVATO RECORD VALIDO
101700050124     C                   MOVEL     'N'           WRECOK
101800050124DO  1C     $Fpes         DOWEQ     'N'
101900050124     C     WRECOK        ANDEQ     'N'
102000050124     C                   READ      SIpes01L                               99
102100050124IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
102200050124     C                   MOVEL     'S'           $Fpes                          *FINE CICLO
102300050124X   2C                   ELSE
102400050124     C                   EXSR      CHKpes                                       *CONTROLLA RECORD
102500050124E   2C                   ENDIF
102600050124E   1C                   ENDDO
102700050124     C*
102800050124     C                   ENDSR
102900050124     C*------------------------------------------------------------------------*
103000050124     C* CHKpes - CONTROLLA VALIDITA' RECORD
103100050124     C*------------------------------------------------------------------------*
103200050124     C     CHKpes        BEGSR
103300050124     C*
103400050124     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
103500050124     C*
103600050124     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
103700050124IF  1C     pesDAT        IFGE      PARDAT
103800050124     C     pesDAT        ANDLE     PARDA2
103900050124X   1C                   ELSE
104000050124     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
104100050124E   1C                   ENDIF
104200050124     C*
104300050124     C                   ENDSR
104400050124     C*------------------------------------------------------------------------*
104500050124     C* AGGpes - SCRIVE / CANCELLA RECORD
104600050124     C*------------------------------------------------------------------------*
104700050124     C     AGGpes        BEGSR
104800050124     C*
104900050124     C* SCRIVE IL RECORD SUL FILE DA SALVARE
105000050124     C                   WRITE     SIpesS                               98
105100050124     C*
105200050124     C* NO ERRORE, CANCELLA RECORD
105300050124IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
105400050124     C                   DELETE    SIpes000
105500050124     C                   ADD       1             TOTpes                         *TOT AGGIORNATI SISD
105600050124     C*
105700050124     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
105800050124X   1C                   ELSE                                                   *ERRORE
105900050124     C                   MOVEL     'W'           WSDX
106000050124     C                   EXSR      STAERR
106100050124     C                   MOVEL     'S'           $FINE                          *FINE CICLO
106200050124E   1C                   ENDIF
106300050124     C*
106400050124     C                   ENDSR
106500050124     C*------------------------------------------------------------------------*
106600050124     C* ELAvlm - ELABORAZIONE PER SIvlm
106700050124     C*------------------------------------------------------------------------*
106800050124     C     ELAvlm        BEGSR
106900050124     C*
107000050124     C* POSIZIONAMENTO E PRIMA LETTURA
107100050124     C                   EXSR      SETvlm
107200050124     C*
107300050124     C* CICLO FINO A FINE CICLO O FINE FILE
107400050124DO  1C     $FINE         DOWEQ     'N'
107500050124     C     $Fvlm         ANDEQ     'N'
107600050124     C*
107700050124     C* SCRIVE / CANCELLA RECORD
107800050124     C                   EXSR      AGGvlm
107900050124     C*
108000050124     C* LETTURA SUCESSIVA
108100050124     C                   EXSR      LETvlm
108200050124     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
108300050124E   1C                   ENDDO                                                  *FINE FILE
108400050124     C*
108500050124     C                   MOVEL     'V'           WSDX                           *Weight
108600050124     C                   EXSR      STATOT
108700050124     C*
108800050124     C                   ENDSR
108900050124     C*------------------------------------------------------------------------*
109000050124     C* SETvlm - POSIZIONAMENTO E PRIMA LETTURA
109100050124     C*------------------------------------------------------------------------*
109200050124     C     SETvlm        BEGSR
109300050124     C*
109400050124     C* REIMPOSTA FINE PGM
109500050124     C                   MOVEL     'N'           $FINE                          *FINE PGM
109600050124     C*
109700050124     C     *LOVAL        SETLL     SIvlm01L                           99
109800050124IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
109900050124     C                   MOVEL     'S'           $Fvlm                          *FINE CICLO
110000050124X   1C                   ELSE
110100050124     C                   EXSR      LETvlm                                       *LETTURA RECORD
110200050124E   1C                   ENDIF
110300050124     C*
110400050124     C                   ENDSR
110500050124     C*------------------------------------------------------------------------*
110600050124     C* LETvlm - LETTURA PROSSIMO RECORD
110700050124     C*------------------------------------------------------------------------*
110800050124     C     LETvlm        BEGSR
110900050124     C*
111000050124     C* LETTURA FINO A:
111100050124     C*  - FINE FILE
111200050124     C*  - TROVATO RECORD VALIDO
111300050124     C                   MOVEL     'N'           WRECOK
111400050124DO  1C     $Fvlm         DOWEQ     'N'
111500050124     C     WRECOK        ANDEQ     'N'
111600050124     C                   READ      SIvlm01L                               99
111700050124IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
111800050124     C                   MOVEL     'S'           $Fvlm                          *FINE CICLO
111900050124X   2C                   ELSE
112000050124     C                   EXSR      CHKvlm                                       *CONTROLLA RECORD
112100050124E   2C                   ENDIF
112200050124E   1C                   ENDDO
112300050124     C*
112400050124     C                   ENDSR
112500050124     C*------------------------------------------------------------------------*
112600050124     C* CHKvlm - CONTROLLA VALIDITA' RECORD
112700050124     C*------------------------------------------------------------------------*
112800050124     C     CHKvlm        BEGSR
112900050124     C*
113000050124     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
113100050124     C*
113200050124     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
113300050124IF  1C     vlmDAT        IFGE      PARDAT
113400050124     C     vlmDAT        ANDLE     PARDA2
113500050124X   1C                   ELSE
113600050124     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
113700050124E   1C                   ENDIF
113800050124     C*
113900050124     C                   ENDSR
114000050124     C*------------------------------------------------------------------------*
114100050124     C* AGGvlm - SCRIVE / CANCELLA RECORD
114200050124     C*------------------------------------------------------------------------*
114300050124     C     AGGvlm        BEGSR
114400050124     C*
114500050124     C* SCRIVE IL RECORD SUL FILE DA SALVARE
114600050124     C                   WRITE     SIvlmS                               98
114700050124     C*
114800050124     C* NO ERRORE, CANCELLA RECORD
114900050124IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
115000050124     C                   DELETE    SIvol000
115100050124     C                   ADD       1             TOTvlm                         *TOT AGGIORNATI SIvl
115200050124     C*
115300050124     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
115400050124X   1C                   ELSE                                                   *ERRORE
115500050124     C                   MOVEL     'V'           WSDX
115600050124     C                   EXSR      STAERR
115700050124     C                   MOVEL     'S'           $FINE                          *FINE CICLO
115800050124E   1C                   ENDIF
115900050124     C*
116000050124     C                   ENDSR
116100051117     C*------------------------------------------------------------------------*
116200051117     C* ELAcop - ELABORAZIONE PER SIcop
116300051117     C*------------------------------------------------------------------------*
116400051117     C     ELAcop        BEGSR
116500051117     C*
116600051117     C* POSIZIONAMENTO E PRIMA LETTURA
116700051117     C                   EXSR      SETcop
116800051117     C*
116900051117     C* CICLO FINO A FINE CICLO O FINE FILE
117000051117DO  1C     $FINE         DOWEQ     'N'
117100051117     C     $Fcop         ANDEQ     'N'
117200051117     C*
117300051117     C* SCRIVE / CANCELLA RECORD
117400051117     C                   EXSR      AGGcop
117500051117     C*
117600051117     C* LETTURA SUCESSIVA
117700051117     C                   EXSR      LETcop
117800051117     C                   SHTDN                                        98        *CHIUSURA CONTROLLAT
117900051117E   1C                   ENDDO                                                  *FINE FILE
118000051117     C*
118100051117     C                   MOVEL     'V'           WSDX                           *Weight
118200051117     C                   EXSR      STATOT
118300051117     C*
118400051117     C                   ENDSR
118500051117     C*------------------------------------------------------------------------*
118600051117     C* SETcop - POSIZIONAMENTO E PRIMA LETTURA
118700051117     C*------------------------------------------------------------------------*
118800051117     C     SETcop        BEGSR
118900051117     C*
119000051117     C* REIMPOSTA FINE PGM
119100051117     C                   MOVEL     'N'           $FINE                          *FINE PGM
119200051117     C*
119300051117     C     *LOVAL        SETLL     SIcop01L                           99
119400051117IF  1C     *IN99         IFEQ      *ON                                          *FILE FILE
119500051117     C                   MOVEL     'S'           $Fcop                          *FINE CICLO
119600051117X   1C                   ELSE
119700051117     C                   EXSR      LETcop                                       *LETTURA RECORD
119800051117E   1C                   ENDIF
119900051117     C*
120000051117     C                   ENDSR
120100051117     C*------------------------------------------------------------------------*
120200051117     C* LETcop - LETTURA PROSSIMO RECORD
120300051117     C*------------------------------------------------------------------------*
120400051117     C     LETcop        BEGSR
120500051117     C*
120600051117     C* LETTURA FINO A:
120700051117     C*  - FINE FILE
120800051117     C*  - TROVATO RECORD VALIDO
120900051117     C                   MOVEL     'N'           WRECOK
121000051117DO  1C     $Fcop         DOWEQ     'N'
121100051117     C     WRECOK        ANDEQ     'N'
121200051117     C                   READ      SIcop01L                               99
121300051117IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
121400051117     C                   MOVEL     'S'           $Fcop                          *FINE CICLO
121500051117X   2C                   ELSE
121600051117     C                   EXSR      CHKcop                                       *CONTROLLA RECORD
121700051117E   2C                   ENDIF
121800051117E   1C                   ENDDO
121900051117     C*
122000051117     C                   ENDSR
122100051117     C*------------------------------------------------------------------------*
122200051117     C* CHKcop - CONTROLLA VALIDITA' RECORD
122300051117     C*------------------------------------------------------------------------*
122400051117     C     CHKcop        BEGSR
122500051117     C*
122600051117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
122700051117     C*
122800051117     C* PERIODO <> QUELLO RICHIESTO, ESCLUDE
122900051117IF  1C     copAAAAmm     IFGE      PARDAT
123000051117     C     copAAAAmm     ANDLE     PARDA2
123100051117X   1C                   ELSE
123200051117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
123300051117E   1C                   ENDIF
123400051117     C*
123500051117     C                   ENDSR
123600051117     C*------------------------------------------------------------------------*
123700051117     C* AGGcop - SCRIVE / CANCELLA RECORD
123800051117     C*------------------------------------------------------------------------*
123900051117     C     AGGcop        BEGSR
124000051117     C*
124100051117     C* SCRIVE IL RECORD SUL FILE DA SALVARE
124200051117     C                   WRITE     SIcopS                               98
124300051117     C*
124400051117     C* NO ERRORE, CANCELLA RECORD
124500051117IF  1C     *IN98         IFEQ      *OFF                                         *NO ERRORE
124600051117     C                   DELETE    SIcop000
124700051117     C                   ADD       1             TOTcop                         *TOT AGGIORNATI SIvl
124800051117     C*
124900051117     C* SI ERRORE, STAMPA ERRORE ED ESCE DAL CICLO
125000051117X   1C                   ELSE                                                   *ERRORE
125100051117     C                   MOVEL     'V'           WSDX
125200051117     C                   EXSR      STAERR
125300051117     C                   MOVEL     'S'           $FINE                          *FINE CICLO
125400051117E   1C                   ENDIF
125500051117     C*
125600051117     C                   ENDSR
125700970723     C*------------------------------------------------------------------------*
125800970723     C* STAERR - STAMPA ERRORE
125900970723     C*------------------------------------------------------------------------*
126000970723     C     STAERR        BEGSR
126100970723     C*
126200970723     C* FINE PAGINA, STAMPA TESTATA
126300970723IF  1C     *INOF         IFEQ      *ON
126400970723     C                   EXCEPT    TEST
126500970723     C                   SETOFF                                       OF
126600970723E   1C                   ENDIF
126700970723     C*
126800990609     C* STAMPA ERRORE PER -SISDA-
126900970723IF  1C     WSDX          IFEQ      'A'
127000970723     C                   EXCEPT    ESDA
127100970723E   1C                   ENDIF
127200970723     C*
127300990609     C* STAMPA ERRORE PER -SISDC-
127400970723IF  1C     WSDX          IFEQ      'C'
127500970723     C                   EXCEPT    ESDC
127600970723E   1C                   ENDIF
127700970723     C*
127800990609     C* STAMPA ERRORE PER -SISDF-
127900970723IF  1C     WSDX          IFEQ      'F'
128000970723     C                   EXCEPT    ESDF
128100970723E   1C                   ENDIF
128200970723     C*
128300040312     C* STAMPA ERRORE PER -SISDO-
128400970723IF  1C     WSDX          IFEQ      'O'
128500040312     C                   EXCEPT    ESDO
128600970723E   1C                   ENDIF
128700970723     C*
128800990609     C* STAMPA ERRORE PER -SISDP-
128900970723IF  1C     WSDX          IFEQ      'P'
129000970723     C                   EXCEPT    ESDP
129100970723E   1C                   ENDIF
129200970723     C*
129300990609     C* STAMPA ERRORE PER -SISDR-
129400970723IF  1C     WSDX          IFEQ      'R'
129500970723     C                   EXCEPT    ESDR
129600970723E   1C                   ENDIF
129700970723     C*
129800990609     C* STAMPA ERRORE PER -SISDT-
129900970723IF  1C     WSDX          IFEQ      'T'
130000970723     C                   EXCEPT    ESDT
130100970723E   1C                   ENDIF
130200040312     C* STAMPA ERRORE PER -SISDE-
130300040312IF  1C     WSDX          IFEQ      'E'
130400040312     C                   EXCEPT    ESDE
130500040312E   1C                   ENDIF
130600970723     C*
130700050124     C* STAMPA ERRORE PER -SIpes-
130800050124IF  1C     WSDX          IFEQ      'W'
130900050124     C                   EXCEPT    Epes
131000050124E   1C                   ENDIF
131100050124     C* STAMPA ERRORE PER -SIvlm-
131200050124IF  1C     WSDX          IFEQ      'V'
131300050124     C                   EXCEPT    Evlm
131400050124E   1C                   ENDIF
131500051117     C*
131600051117     C* STAMPA ERRORE PER -SIcop-
131700051117IF  1C     WSDX          IFEQ      'V'
131800051117     C                   EXCEPT    Ecop
131900051117E   1C                   ENDIF
132000050124     C*
132100970723     C                   ENDSR
132200970723     C*------------------------------------------------------------------------*
132300970723     C* STATOT - STAMPA RIEPILOGO ELABORAZIONE
132400970723     C*------------------------------------------------------------------------*
132500970723     C     STATOT        BEGSR
132600970723     C*
132700970723     C* FINE PAGINA, STAMPA TESTATA
132800970723IF  1C     *INOF         IFEQ      *ON
132900970723     C                   EXCEPT    TEST
133000970723     C                   SETOFF                                       OF
133100970723E   1C                   ENDIF
133200970723     C*
133300990609     C* STAMPA ERRORE PER -SISDA-
133400970723IF  1C     WSDX          IFEQ      'A'
133500970723     C                   EXCEPT    TSDA
133600970723E   1C                   ENDIF
133700970723     C*
133800990609     C* STAMPA ERRORE PER -SISDC-
133900970723IF  1C     WSDX          IFEQ      'C'
134000970723     C                   EXCEPT    TSDC
134100970723E   1C                   ENDIF
134200970723     C*
134300990609     C* STAMPA ERRORE PER -SISDF-
134400970723IF  1C     WSDX          IFEQ      'F'
134500970723     C                   EXCEPT    TSDF
134600970723E   1C                   ENDIF
134700970723     C*
134800990609     C* STAMPA ERRORE PER -SISDO-
134900970723IF  1C     WSDX          IFEQ      'O'
135000970723     C                   EXCEPT    TSDO
135100970723E   1C                   ENDIF
135200970723     C*
135300990609     C* STAMPA ERRORE PER -SISDP-
135400970723IF  1C     WSDX          IFEQ      'P'
135500970723     C                   EXCEPT    TSDP
135600970723E   1C                   ENDIF
135700970723     C*
135800990609     C* STAMPA ERRORE PER -SISDR-
135900970723IF  1C     WSDX          IFEQ      'R'
136000970723     C                   EXCEPT    TSDR
136100970723E   1C                   ENDIF
136200970723     C*
136300990609     C* STAMPA ERRORE PER -SISDT-
136400970723IF  1C     WSDX          IFEQ      'T'
136500970723     C                   EXCEPT    TSDT
136600970723E   1C                   ENDIF
136700040312     C*
136800040312     C* STAMPA ERRORE PER -SISDE-
136900040312IF  1C     WSDX          IFEQ      'E'
137000040312     C                   EXCEPT    TSDE
137100040312E   1C                   ENDIF
137200050124     C*
137300050124     C* STAMPA ERRORE PER -SIpes-
137400050124IF  1C     WSDX          IFEQ      'W'
137500050124     C                   EXCEPT    Tpes
137600050124E   1C                   ENDIF
137700050124     C*
137800050124     C* STAMPA ERRORE PER -SIvlm-
137900050124IF  1C     WSDX          IFEQ      'V'
138000050124     C                   EXCEPT    Tvlm
138100050124E   1C                   ENDIF
138200051117     C*
138300051117     C* STAMPA ERRORE PER -SIcop-
138400051117IF  1C     WSDX          IFEQ      'V'
138500051117     C                   EXCEPT    Tcop
138600051117E   1C                   ENDIF
138700970723     C*
138800970723     C                   ENDSR
138900970723     C*------------------------------------------------------------------------*
139000970723     C* FINSR - OPERAZIONI FINALI
139100970723     C*------------------------------------------------------------------------*
139200970723     C     FINSR         BEGSR
139300970723     C*
139400970723     C* AGGIORNA LA TABELLE DELLE DATE DI TRASFERIMENTO
139500020920     C                   Z-ADD     1             KTBKUT
139600020701     C                   MOVEL     '5S'          KTBCOD
139700970723     C     KEYTAB        CHAIN     TABEL00F                           99
139800970723IF  1C     *IN99         IFEQ      *OFF
139900020701     C                   MOVEL     TBLUNI        DS5S
140000020701     C                   MOVEL     PARDA2        §5SDCC
140100020701     C                   MOVE      01            §5SDCC
140200020701     C                   MOVEL     DS5S          TBLUNI
140300970723     C                   UPDATE    TABEL
140400970723E   1C                   ENDIF
140500970723     C*
140600970723     C                   ENDSR
140700020920     C*--------------------------------------------------------------------------------------------*
140800020920     C* REPERISCE I DATI UTENTE
140900020920     C*--------------------------------------------------------------------------------------------*
141000020920     C     REPDATIUTE    BEGSR
141100020920     C*
141200020920     C* INIZIALIZZA VARIABILI DI WRK
141300020920     C                   CLEAR                   TIBS34DS
141400020920     C                   CLEAR                   AZUTEDS
141500020920     C                   CLEAR                   DDATIUTE
141600020920     C*
141700020920     C     *DTAARA       DEFINE    §azute        azuteds
141800020920     C     *DTAARA       DEFINE    §datiute      ddatiute
141900020920     C                   IN(E)     *DTAARA
142000020920if  1C                   IF        %Error
142100020920     c                   EVAL      I34Tla = 'L'
142200020920     C                   CALL      'TIBS34R'
142300020920     C                   PARM                    Tibs34Ds
142400020920     C                   IN        *DTAARA
142500020920e   1C                   ENDIF
142600020920     C*
142700020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
142800020920     C                   MOVEL(P)  RSUT          DSFIRS           20
142900020920     C*
143000020920     C                   ENDSR
143100970723     C*------------------------------------------------------------------------*
143200970723     C* *INZSR - OPERAZIONI INIZIALI
143300970723     C*------------------------------------------------------------------------*
143400970723     C     *INZSR        BEGSR
143500970723     C*--------------------
143600970723     C* RICEVIMENTO PARAMETRI
143700970723     C*--------------------
143800970723     C     *ENTRY        PLIST
143900970723     C                   PARM                    KPJBA
144000970723     C                   MOVEL     KPJBU         PARAM
144100970723     C*--------------------
144200970723     C* VARIABILI RIFERITE AL DATABASE
144300970723     C*--------------------
144400970723     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
144500970723     C     *LIKE         DEFINE    TBLCOD        KTBCOD
144600970723     C*--------------------
144700970723     C* CHAIVI DI LETTURA
144800970723     C*--------------------
144900970723     C* LETTURA TABEL00F - COMPLETA
145000970723     C     KEYTAB        KLIST
145100970723     C                   KFLD                    KTBKUT                         *CODICE UTENTE
145200970723     C                   KFLD                    KTBCOD                         *CODICE TABELLA
145300970723     C*--------------------
145400970723     C* VARIABILI DI CONTROLLO
145500970723     C*--------------------
145600970723     C                   MOVEL     'N'           $FINE             1            *FINE PGM
145700970723     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
145800990609     C                   MOVEL     'N'           $FSDA             1            *FINE LETTURA SISDA
145900990609     C                   MOVEL     'N'           $FSDC             1            *FINE LETTURA SISDC
146000990609     C                   MOVEL     'N'           $FSDF             1            *FINE LETTURA SISDF
146100990609     C                   MOVEL     'N'           $FSDO             1            *FINE LETTURA SISDO
146200990609     C                   MOVEL     'N'           $FSDP             1            *FINE LETTURA SISDP
146300990609     C                   MOVEL     'N'           $FSDR             1            *FINE LETTURA SISDR
146400990609     C                   MOVEL     'N'           $FSDT             1            *FINE LETTURA SISDT
146500040312     C                   MOVEL     'N'           $FSDe             1            *FINE LETTURA SISDO
146600050124     C                   MOVEL     'N'           $FPes             1            *FINE LETTURA SIpes
146700050124     C                   MOVEL     'N'           $Fvlm             1            *FINE LETTURA SIvlm
146800051117     C                   MOVEL     'N'           $Fcop             1            *FINE LETTURA SIcop
146900970723     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
147000990609     C                   Z-ADD     *ZEROS        TOTSDA            9 0          *TOT AGGIORNATI SISD
147100990609     C                   Z-ADD     *ZEROS        TOTSDC            9 0          *TOT AGGIORNATI SISD
147200990609     C                   Z-ADD     *ZEROS        TOTSDF            9 0          *TOT AGGIORNATI SISD
147300990609     C                   Z-ADD     *ZEROS        TOTSDO            9 0          *TOT AGGIORNATI SISD
147400990609     C                   Z-ADD     *ZEROS        TOTSDP            9 0          *TOT AGGIORNATI SISD
147500990609     C                   Z-ADD     *ZEROS        TOTSDR            9 0          *TOT AGGIORNATI SISD
147600990609     C                   Z-ADD     *ZEROS        TOTSDT            9 0          *TOT AGGIORNATI SISD
147700040312     C                   Z-ADD     *ZEROS        TOTSDE            9 0          *TOT AGGIORNATI SISD
147800050124     C                   Z-ADD     *ZEROS        totpes            9 0          *TOT AGGIORNATI SIpe
147900050124     C                   Z-ADD     *ZEROS        totvlm            9 0          *TOT AGGIORNATI SIvl
148000051117     C                   Z-ADD     *ZEROS        totcop            9 0          *TOT AGGIORNATI SIco
148100990609     C                   MOVEL     *BLANKS       WSDX              1            *ERRORE DI SISDxx
148200970908     C                   MOVEL     '1'           *INOF                          *OVERFLOW
148300970723     C*
148400970723     C* REPERISCE I DATI DELL'UTENTE
148500020920     C                   EXSR      REPDATIUTE
148600970723     C*
148700970723     C* REPERIMENTO DATA E ORA CORRENTE
148800970723     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
148900970723     C                   MOVE      WN14          DATCOR            8 0          *DATA (8) IN G/M/AA
149000970723     C                   MOVEL     WN14          ORACOR            6 0          *ORA (6)
149100970723     C*
149200970723     C                   ENDSR
149300970723     O*------------------------------------------------------------------------*
149400970724     OQSYSPRT   E            TEST             02
149500970723     O                       RIG(1)              66
149600970723     O                       RIG(2)             132
149700970723     O                       DSFIRS              20
149800970723     O                       DATCOR             107 '  /  /    '
149900970723     O                       ORACOR             118 '  :  :  '
150000970723     O                       PAGE          Z    132
150100970723     O*------------------------
150200970723     O          E            TEST             03
150300970723     O                       RIG(3)              66
150400970723     O                       RIG(3)             132
150500970723     O*------------------------
150600970723     O          E            ESDA        1
150700970723     O                       RIG(4)              66
150800990526     O                       SDAKSC        Z     23
150900990526     O                       SDAANN        Z   +  1
151000990526     O                       SDAMES        Z   +  1
151100990526     O                       SDADLV        Z   +  1
151200990526     O                       SDATSP            +  1
151300970723     O*------------------------
151400970723     O          E            TSDA        1
151500970723     O                       RIG(5)              66
151600970723     O                       TOTSDA        Z     39
151700970723     O*------------------------
151800970723     O          E            ESDC        1
151900970723     O                       RIG(6)              66
152000970723     O                       SDCKSC        Z     23
152100990526     O                       SDCANN        Z   +  1
152200990526     O                       SDCMES        Z   +  1
152300990526     O                       SDCREP        Z   +  1
152400990526     O                       SDCREA        Z   +  1
152500990526     O                       SDCFPE            +  1
152600990526     O                       SDCTSP            +  1
152700990526     O                       SDCCTR        Z   +  1
152800990526     O                       SDCTPO            +  1
152900990526     O                       SDCRBL            +  1
153000990526     O                       SDCTPR            +  1
153100970723     O*------------------------
153200970723     O          E            TSDC        1
153300970723     O                       RIG(7)              66
153400970723     O                       TOTSDC        Z     39
153500970723     O*------------------------
153600970723     O          E            ESDF        1
153700970723     O                       RIG(8)              66
153800990526     O                       SDFANN        Z     20
153900990526     O                       SDFMES        Z   +  1
154000990526     O                       SDFFIL        Z   +  1
154100970723     O*------------------------
154200970723     O          E            TSDF        1
154300970723     O                       RIG(9)              66
154400970723     O                       TOTSDF        Z     39
154500970723     O*------------------------
154600970723     O          E            ESDO        1
154700970723     O                       RIG(10)             66
154800970723     O                       SDOKSC        Z     23
154900990526     O                       SDOANN        Z   +  1
155000990526     O                       SDOMES        Z   +  1
155100970723     O*------------------------
155200970723     O          E            TSDO        1
155300970723     O                       RIG(11)             66
155400970723     O                       TOTSDO        Z     39
155500970723     O*------------------------
155600970723     O          E            ESDP        1
155700970723     O                       RIG(12)             66
155800970723     O                       SDPKSC        Z     23
155900990526     O                       SDPANN        Z   +  1
156000990526     O                       SDPMES        Z   +  1
156100990526     O                       SDPCTR        Z   +  1
156200990526     O                       SDPSVR            +  1
156300990526     O                       SDPFTC            +  1
156400970723     O*------------------------
156500970723     O          E            TSDP        1
156600970723     O                       RIG(13)             66
156700970723     O                       TOTSDP        Z     39
156800970723     O*------------------------
156900970723     O          E            ESDR        1
157000970723     O                       RIG(14)             66
157100990526     O                       SDRANN        Z     20
157200990526     O                       SDRFLP        Z   +  1
157300990526     O                       SDRFLA        Z   +  1
157400990526     O                       SDRMES        Z   +  1
157500990526     O                       SDRFPE            +  1
157600990526     O                       SDRTSP            +  1
157700990526     O                       SDRTPO            +  1
157800990526     O                       SDRRBL            +  1
157900990526     O                       SDRTPR            +  1
158000970723     O*------------------------
158100970723     O          E            TSDR        1
158200970723     O                       RIG(15)             66
158300970723     O                       TOTSDR        Z     39
158400970723     O*------------------------
158500970723     O          E            ESDT        1
158600970723     O                       RIG(16)             66
158700990526     O                       SDTANN        Z     20
158800990526     O                       SDTTFP        Z   +  1
158900990526     O                       SDTTFA        Z   +  1
159000990526     O                       SDTFPE            +  1
159100990526     O                       SDTMES        Z   +  1
159200990526     O                       SDTDEC            +  1
159300990526     O                       SDTTT1        Z   +  1
159400990526     O                       SDTTT2        Z   +  1
159500040524     O                       SDTTT3        Z   +  1
159600040524     O                       SDTTT4        Z   +  1
159700040524     O                       SDTTT5        Z   +  1
159800970723     O*------------------------
159900970723     O          E            TSDT        1
160000970723     O                       RIG(17)             66
160100970723     O                       TOTSDT        Z     39
160200040312     O*------------------------
160300040312     O          E            ESDE        1
160400040312     O                       RIG(18)             66
160500040312     O                       SDeKSC        Z     23
160600040312     O                       SDeANN        Z   +  1
160700040312     O                       SDeMES        Z   +  1
160800040312     O*------------------------
160900040312     O          E            TSDe        1
161000040312     O                       RIG(19)             66
161100040312     O                       TOTSDe        Z     39
161200050124     O*------------------------
161300050124     O          E            EPES        1
161400050124     O                       RIG(21)             66
161500050124     O                       pesKSC        Z     23
161600050124     O                       pesANN        Z   +  1
161700050124     O                       pesMES        Z   +  1
161800050124     O*------------------------
161900050124     O          E            TPES        1
162000050124     O                       RIG(22)             66
162100050124     O                       TOTpes        Z     39
162200050124     O*------------------------
162300050124     O          E            EVLM        1
162400050124     O                       RIG(23)             66
162500050124     O                       vlmKSC        Z     23
162600050124     O                       vlmANN        Z   +  1
162700050124     O                       vlmMES        Z   +  1
162800050124     O*------------------------
162900050124     O          E            TVLM        1
163000050124     O                       RIG(24)             66
163100050124     O                       TOTvlm        Z     39
163200051117     O*------------------------
163300051117     O          E            ECOP        1
163400051117     O                       RIG(25)             66
163500051117     O                       copAAS        Z     23
163600051117     O                       copMES        Z   +  1
163700051117     O*------------------------
163800051117     O          E            TCOP        1
163900051117     O                       RIG(26)             66
164000051117     O                       TOTcop        Z     39
164100970723     O*------------------------------------------------------------------------*
164200970723** RIG - RIGHE DI STAMPA
164300970723XXXXXXXXXXXXXXXXXXXX            TNSS42R               *** CANCELLA1
164400970724ZIONE SALDI DI SEDE ***        99/99/9999   66:66:66      PAG.99992
164500970723------------------------------------------------------------------3
164600990609ERRORE SISDA -> 9999999 9999 99 999 X                             4
164700990609RECORD AGGIORNATI IN SISDA -> 999999999                           5
164800990609ERRORE SISDC -> 9999999 9999 99 999 999 XX X 999 X X X            6
164900990609RECORD AGGIORNATI IN SISDC -> 999999999                           7
165000990609ERRORE SISDF -> 9999 99 999                                       8
165100990609RECORD AGGIORNATI IN SISDF -> 999999999                           9
165200990609ERRORE SISDO -> 9999999 9999 99                                   10
165300990609RECORD AGGIORNATI IN SISDO -> 999999999                           11
165400990609ERRORE SISDP -> 9999999 9999 99 999 X X                           12
165500990609RECORD AGGIORNATI IN SISDP -> 999999999                           13
165600990609ERRORE SISDR -> 9999 999 999 99 XX X X X X                        14
165700990609RECORD AGGIORNATI IN SISDR -> 999999999                           15
165800990609ERRORE SISDT -> 9999 999 999 XX 99 X 999 999                      16
165900990609RECORD AGGIORNATI IN SISDT -> 999999999                           17
166000040312ERRORE SISDE -> 9999999 9999 99                                   18
166100040312RECORD AGGIORNATI IN SISDE -> 999999999                           19
16620097072320
166300050124ERRORE SIPES -> 9999999 9999 99                                   21
166400050124RECORD AGGIORNATI IN SIPES -> 999999999                           22
166500050124ERRORE SIVLM -> 9999999 9999 99                                   23
166600050124RECORD AGGIORNATI IN SIVLM -> 999999999                           24
166700051117ERRORE SICOP ->    9999 99                                        25
166800051117RECORD AGGIORNATI IN SICOP -> 999999999                           26
