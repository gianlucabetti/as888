000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020418      *------------------------------------------------------------------------*
000300020418      * STAMPA TERMINAL STANDARD / ECCEZIONALI + ESTENSIONE (ESEGUE)
000400020418      *------------------------------------------------------------------------*
000500020418      * NOTE: LA LOGICA DI STAMPA (X GLI ARRIVI) SEGUE IL SEGUENTE CRITERIO
000600020418      *       => Se DECFIL = 0 - EPA = 'A'
000700020418      *                        - TFA = DECATE
000800020418      *                        - TFP = DECATP
000900020418      *                        - TFE = DECFIL
001000020418      *
001100020418      *       => Se DECFIL > 0 - EPA = 'A'
001200020418      *                        - TFA = DECFIL
001300020418      *                        - TFP = 0
001400020418      *                        - TFE = DECATS
001500020418      *------------------------------------------------------------------------*
001600970911     F*------------------------------------------------------------------------*
001700970911     F* DATA BASE
001800970911     F*------------------------------------------------------------------------*
001900020531     fazorg01l  if   e           k disk
002000970911     FAZCAE01L  IF   E           K DISK
002100970911     FAZCAE02L  IF   E           K DISK
002200970911     F                                     RENAME(AZCAE000:AZCAE2)
002300000428     FAZPAE01L  IF   E           K DISK
002400970911     FTISI20P   O    E             PRINTER OFLIND(*IN01)
002500970911     D*------------------------------------------------------------------------*
002600970911     D* SCHIERE
002700970911     D*------------------------------------------------------------------------*
002800020531
002900970911     D*-------------------
003000970911     D* ECCEZIONI
003100970911     D*-------------------
003200060706     D PTFA            S              3  0 DIM(200)                             *ARRIVO
003300060706     D PTFE            S              3  0 DIM(200)                             *STANDARD/ECCEZIONE
003400060706     D ATFP            S              3  0 DIM(200)                             *PARTENZA
003500060706     D ATFE            S              3  0 DIM(200)                             *STANDARD/ECCEZIONE
003600060706     d stfp            s              3  0 dim(200)                             *partenza
003700060706     d stfe            s              3  0 dim(200)                             *standard/eccesione
003701060706     d OtfA            s              3  0 dim(200)                             *partenza
003702060706     d Otfe            s              3  0 dim(200)                             *standard/eccesione
003800970911     D*------------------------------------------------------------------------*
003900970911     D* INPUT
004000970911     D*------------------------------------------------------------------------*
004100020531
004200020531     d i               s              3  0
004300020531     d ii              s              3  0
004400020531
004500970911     D*-------------------
004600970911     D* DS PARAMETRI DI LANCIO PGM
004700970911     D*-------------------
004800970911     D PARAM           DS
004900970911     D  PARDAT                 1      8  0
005000970911     D*----------------
005100970911     D* DS CONTROLLO DATE
005200970911     D*----------------
005300970911     D WLBDA8          DS
005400970911     D  G08DAT                 1      8  0
005500970911     D  G08INV                 9     16  0
005600970911     D  G08ERR                17     17
005700970911     D  G08TGI                18     22  0
005800971128     D*------------
005900971128     D* DS REPEIMENTO TERMINALs
006000971128     D*------------
006100971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006200970911     D*-------------------
006300970911     D* ARCHITETTURA
006400970911     D*-------------------
006500970911     D KPJBA         E DS
006600020531
006700020531     d trul26ds      e ds
006800020531     d  skpo                  11    760  0 dim(250)
006900020603     d  skpot                761   1510  0 dim(250)
007000970911     C*------------------------------------------------------------------------*
007100970911     C* MAIN LINE
007200970911     C*------------------------------------------------------------------------*
007300970911     C*
007400970911     C* STAMPA I PARAMETRI DI LANCIO
007500970911     C                   EXSR      STAPAR
007600020531
007700020531      * Leggo la schiera dei terminal caricati
007800020531     c                   do        250           ii
007900020531     c                   if        skpo(ii) = *zeros
008000020531     c                   leave
008100020531     c                   endif
008200020531      * controllo se p.o. è ok
008300020531     c     skpo(ii)      chain     azorg01l
008400020531     c                   if        not%found(azorg01l) or orgfva <> *blanks
008500020531     c                             or orgfag = 'V'
008600020531     c                   iter
008700020531     c                   endif
008800970911     C*
008900970911     C* OPERAZIONI PER NUOVA DESCRIZIONE FILIALE
009100970911     C                   EXSR      NEWDES
009600970911     C*
009700970911     C* MEMCAE - MEMORIZZA I TERMINAL STANDARD E ECCEZIONALI DELLA FILIALE
009800970911     C                   EXSR      MEMCAE
010000970911     C*
010100970911     C* STAMPA DATI FILIALE
010300970911     C                   EXSR      STAFIL
010500970911E   1C                   ENDDO                                                  *FINE FILE
010600970911     C*
010700970911     C* STAMPA RIGA DI FINE STAMPA
010800970911     C                   WRITE     SI20FI
010900971128     C*
011000971128     C* OPERAZIONI FINALI
011100971128     C                   EXSR      FINSR
011200970911     C*
011300970911     C                   SETON                                        LR
011400971128     C*------------------------------------------------------------------------*
011500971128     C* FINSR - OPERAZIONI FINALI
011600971128     C*------------------------------------------------------------------------*
011700971128     C     FINSR         BEGSR
011800971128     C*
011900971128     C                   CLEAR                   DSLV55
012000971128     C                   MOVEL     'C'           D55TLA
012100971128     C                   CALL      'FNLV55R'
012200971128     C                   PARM                    DSLV55
012300971128     C*
012400971128     C                   ENDSR
012500970911     C*------------------------------------------------------------------------*
012600970911     C* NEWDES - OPERAZIONI PER NUOVA DESCRIZIONE FILIALE
012700970911     C*------------------------------------------------------------------------*
012800970911     C     NEWDES        BEGSR
012900970911     C*
013000001211     C* MEMORIZZA ALTRI DATI DI USO FUTURO (CODICE FILIALE)
013100001211     C                   Z-ADD     ORGFIL        DEPFIL
013200001211     C*
013300970911     C* MEMORIZZA LA NUOVA DESCRIZIONE FILIALE IN UN DEPOSITO
013400970911     C                   MOVEL     ORGDES        DEPDES
014700970911     C*
014800970911     C* AZZERA LE VARIABILI DI MEMORIZZAZIONE
014900970911     C                   CLEAR                   PTFA                            *ARRIVO
015000970911     C                   CLEAR                   PTFE                            *STANDARD/ECCEZIONE
015100970911     C                   CLEAR                   ATFP                            *PARTENZA
015200970911     C                   CLEAR                   ATFE                            *STANDARD/ECCEZIONE
015300020418     c                   clear                   stfp                            *partenza
015400020418     c                   clear                   stfe                            *standard/eccezione
015401060706     c                   clear                   OtfA                            *partenza
015402060706     c                   clear                   Otfe                            *standard/eccezione
015500970911     C*
015600970911     C                   ENDSR
015700970911     C*------------------------------------------------------------------------*
015800970911     C* MEMCAE - MEMORIZZA I TERMINAL STANDARD E ECCEZIONALI DELLA FILIALE
015900970911     C*------------------------------------------------------------------------*
016000970911     C     MEMCAE        BEGSR
016100970911     C*---
016200970911     C* P A R T E N Z A
016300970911     C*---
016400970911     C* CARICA I TERMINAL PARTENZA
016500970911     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
016600970911     C                   MOVEL     'P'           KCAEPA
016700970911     C                   Z-ADD     ORGFIL        KCATFP
016800970911     C     KEYCAE        SETLL     AZCAE01L
016900970911     C     KEYCAE        READE     AZCAE01L                               98
017000970911DO  1C     *IN98         DOWEQ     *OFF
017100970911     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
017200970911IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
017300970911     C                   ADD       1             I
017400970911     C                   Z-ADD     CAETFA        PTFA(I)                        *ARRIVO
017500970911     C                   Z-ADD     CAETFE        PTFE(I)                        *STANDAR/ECCEZIONE
017600970911E   2C                   ENDIF
017700970911     C     KEYCAE        READE     AZCAE01L                               98
017800970911E   1C                   ENDDO
017900970911     C*
018000970911     C* SPOSTA IL TERMINAL STANDARD PARTENZA IN 1^ POSIZIONE
018100970911DO  1C     1             DO        I             J                 5 0
018200970911IF  2C     PTFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
018300970911IF  3C     PTFA(J)       IFEQ      *ZEROS                                       *STANDARD PARTENZA
018400970911     C                   Z-ADD     PTFA(J)       PTFA(1)                        *SPOSTA IN 1^POS.
018500970911     C                   Z-ADD     PTFE(J)       PTFE(1)
018600970911     C                   Z-ADD     *ZEROS        PTFA(J)                        *CANCELLA ELEMENTO
018700970911     C                   Z-ADD     *ZEROS        PTFE(J)
018800970911     C                   LEAVE
018900970911E   3C                   ENDIF
019000970911E   2C                   ENDIF
019100970911E   1C                   ENDDO
019200970911     C*---
019300970911     C* A R R I V O
019400970911     C*---
019500970911     C* CARICA I TERMINAL ARRIVO
019600970911     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
019700970911     C                   MOVEL     'A'           KCAEPA
019800970911     C                   Z-ADD     ORGFIL        KCATFA
019900970911     C     KE2CAE        SETLL     AZCAE02L
020000970911     C     KE2CAE        READE     AZCAE02L                               98
020100970911DO  1C     *IN98         DOWEQ     *OFF
020200970911     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
020300970911IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
020400970911     C                   ADD       1             I
020500970911     C                   Z-ADD     CAETFP        ATFP(I)                        *PARTENZA
020600970911     C                   Z-ADD     CAETFE        ATFE(I)                        *STANDAR/ECCEZIONE
020700970911E   2C                   ENDIF
020800970911     C     KE2CAE        READE     AZCAE02L                               98
020900970911E   1C                   ENDDO
021000970911     C*
021100970911     C* SPOSTA IL TERMINAL STANDARD ARRIVO IN 1^ POSIZIONE
021200970911DO  1C     1             DO        I             J
021300970911IF  2C     ATFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
021400970911IF  3C     ATFP(J)       IFEQ      *ZEROS                                       *STANDARD ARRIVO
021500970911     C                   Z-ADD     ATFP(J)       ATFP(1)                        *SPOSTA IN 1^POS.
021600970911     C                   Z-ADD     ATFE(J)       ATFE(1)
021700970911     C                   Z-ADD     *ZEROS        ATFP(J)                        *CANCELLA ELEMENTO
021800970911     C                   Z-ADD     *ZEROS        ATFE(J)
021900970911     C                   LEAVE
022000970911E   3C                   ENDIF
022100970911E   2C                   ENDIF
022200970911E   1C                   ENDDO
022300020418      *---
022400020418      * Tipo terminal "6"
022500020418      *---
022600020418      * Carico i terminal tipo "6"
022700020418     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
022800020418     C                   MOVEL     '6'           KCAEPA
022900020418     C                   Z-ADD     ORGFIL        KCATFA
023000020418     C     KE2CAE        SETLL     AZCAE02L
023100020418     C     KE2CAE        READE     AZCAE02L                               98
023200020418DO  1C     *IN98         DOWEQ     *OFF
023300020418     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
023400020418IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
023500020418     C                   ADD       1             I
023600020418     C                   Z-ADD     CAETFP        STFP(I)                        *PARTENZA
023700020418     C                   Z-ADD     CAETFE        STFE(I)                        *STANDARD/ECCEZIONE
023800020418E   2C                   ENDIF
023900020418     C     KE2CAE        READE     AZCAE02L                               98
024000020418E   1C                   ENDDO
024100020418     C*
024200020418     C* SPOSTA IL TERMINAL TIPO "6" IN 1^ POSIZIONE
024300020418DO  1C     1             DO        I             J
024400020418IF  2C     STFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
024500020418IF  3C     STFP(J)       IFEQ      *ZEROS                                       *STANDARD
024600020418     C                   Z-ADD     STFP(J)       STFP(1)                        *SPOSTA IN 1^POS.
024700020418     C                   Z-ADD     STFE(J)       STFE(1)
024800020418     C                   Z-ADD     *ZEROS        STFP(J)                        *CANCELLA ELEMENTO
024900020418     C                   Z-ADD     *ZEROS        STFE(J)
025000020418     C                   LEAVE
025100020418E   3C                   ENDIF
025200020418E   2C                   ENDIF
025300020418E   1C                   ENDDO
025301060706     C*---
025302060706     C* OTTICO CARICO ECCEZIONE SE C'E'
025303060706     C*---
025304060706     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
025305060706     C                   MOVEL     'O'           KCAEPA
025306060706     C                   Z-ADD     ORGFIL        KCATFP
025307060706     C     KEYCAE        SETLL     AZCAE01L
025308060706     C     KEYCAE        READE     AZCAE01L                               98
025309060706DO  1C     *IN98         DOWEQ     *OFF
025310060706     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
025311060706IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
025312060706     C                   ADD       1             I
025313060706     C                   Z-ADD     CAETFA        OTFA(I)                        *ARRIVO
025314060706     C                   Z-ADD     CAETFE        OTFE(I)                        *STANDAR/ECCEZIONE
025315060706E   2C                   ENDIF
025316060706     C     KEYCAE        READE     AZCAE01L                               98
025317060706E   1C                   ENDDO
025318060706     C*
025319060706     C* SPOSTA IL TERMINAL STANDARD PARTENZA IN 1^ POSIZIONE
025320060706DO  1C     1             DO        I             J                 5 0
025321060706IF  2C     OTFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
025322060706IF  3C     OTFA(J)       IFEQ      *ZEROS                                       *STANDARD PARTENZA
025323060706     C                   Z-ADD     OTFA(J)       OTFA(1)                        *SPOSTA IN 1^POS.
025324060706     C                   Z-ADD     OTFE(J)       OTFE(1)
025325060706     C                   Z-ADD     *ZEROS        OTFA(J)                        *CANCELLA ELEMENTO
025326060706     C                   Z-ADD     *ZEROS        OTFE(J)
025327060706     C                   LEAVE
025328060706E   3C                   ENDIF
025329060706E   2C                   ENDIF
025330060706E   1C                   ENDDO
025400970911     C*
025500970911     C                   ENDSR
025600970911     C*------------------------------------------------------------------------*
025700970911     C* CHKCAE - CONTROLLA VALIDITA' RECORD -AZCAE-
025800970911     C*------------------------------------------------------------------------*
025900970911     C     CHKCAE        BEGSR
026000970911     C*
026100970911     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
026200970911     C*
026300970911     C* RECORD ANNULLATO, ESCLUDE
026400970911IF  1C     CAEATB        IFNE      *BLANKS
026500970911     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026600970911E   1C                   ENDIF
026700970911     C*
026800970911     C* RECORD NON IN DECORRENZA, ESCLUDE
026900970911IF  1C     PARDAT        IFGE      CAEDDE
027000970911     C     PARDAT        ANDLE     CAEDSC
027100970911X   1C                   ELSE
027200970911     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027300970911E   1C                   ENDIF
027400970911     C*
027500970911     C                   ENDSR
027600970911     C*------------------------------------------------------------------------*
027700000428     C* STAFIL - STAMPA DATI DI FILIALE
027800970911     C*------------------------------------------------------------------------*
027900970911     C     STAFIL        BEGSR
028000970911     C*
028100970911     C* SE FINE PAGINA, STAMPA TESTATA
028200970911     C                   EXSR      STATES
028300970911     C*
028400970911     C* STAMPA I TERMINAL STANDARD DELLA FILIALE
028500970911     C                   CLEAR                   SI20DE                         *PULISCE FORMATO
028600970911     C                   Z-ADD     DEPFIL        DECFIL                         *FILIALE
028700970911     C                   Z-ADD     DECFIL        WFIL
028800970911     C                   MOVEL     *BLANKS       WDFIL
028900970911     C                   EXSR      DESFIL
029000970911     C                   MOVEL     WDFIL         DEDFIL
029700060706     C* TERMINAL DI ARRIVO STD
029800970911     C                   Z-ADD     ATFE(1)       DECATS                         TERMINAL ARRIVO STD
029900970911     C                   Z-ADD     DECATS        WFIL
030000970911     C                   MOVEL     *BLANKS       WDFIL
030100970911     C                   EXSR      DESFIL
030200970911     C                   MOVEL     WDFIL         DEDATS
030300060706     C* TERMINAL  DI PARTENZA STD
030400970911     C                   Z-ADD     PTFE(1)       DECPTS                         TERMINAL PARTENZA ST
030500970911     C                   Z-ADD     DECPTS        WFIL
030600970911     C                   MOVEL     *BLANKS       WDFIL
030700970911     C                   EXSR      DESFIL
030800970911     C                   MOVEL     WDFIL         DEDPTS
030801060706     C* OTTICO ECCEZIONE (CE NE È SOLO UNO)
030802060706     C                   Z-ADD     OTFE(1)       DECOTT                         TERMINAL PARTENZA ST
030803060706     C                   Z-ADD     DECott        WFIL
030804060706     C                   MOVEL     *BLANKS       WDFIL
030805060706     C                   EXSR      DESFIL
030806060706     C                   MOVEL     WDFIL         DEDOTT
030900060706     C* P.O. £6
031000020418     C                   Z-ADD     STFE(1)       DECTE6                         TERMINAL "6" STD
031100020418     C                   Z-ADD     DECTE6        WFIL
031200020418     C                   MOVEL     *BLANKS       WDFIL
031300020418     C                   EXSR      DESFIL
031400020418     C                   MOVEL     WDFIL         DEDTE6
031500980602     C*
031600980602     C* ESCLUSIONE DEI PUNTI OPERATIVI SENZA TERMINAL ARRIVO E PARTENZA
031700980602IF  1C     PTFE(1)       IFGT      *ZEROS
031800980602     C     ATFE(1)       ORGT      *ZEROS
031900020418     C     STFE(1)       ORGT      *ZEROS
032000000428     C*
032100000428     C* REPERISCO DALL'ESTENSIONE ECCEZIONI TERMINAL X POSTE IL RAGGRUPPAMENTO SMISTAMENTO GABBIE
032200000428     C                   EXSR      RTVRAG
032300970911     C                   WRITE     SI20DE                                       *DETTAGLIO
032400980602E   1C                   ENDIF
032500970911     C*
032600970911     C* LEGGE TUTTI I TERMINAL PARTENZA E ARRIVO MEMORIZZATI
032700060706DO  1C     2             DO        200           I
032800970911IF  2C     PTFE(I)       IFGT      *ZEROS
032900970911     C     ATFE(I)       ORGT      *ZEROS
033000020418     C     STFE(I)       ORGT      *ZEROS
033100970911     C                   EXSR      STATES                                       *STAMPA TESTATA
033200970911     C                   CLEAR                   SI20DE                         *PULISCE FORMATO
033300970911     C* ECCEZIONE ARRIVO
033400970911IF  3C     ATFE(I)       IFGT      *ZEROS
033500970911     C                   Z-ADD     ATFP(I)       DECATP                         *PARTENZA
033600970911     C                   Z-ADD     DECATP        WFIL
033700970911     C                   MOVEL     *BLANKS       WDFIL
033800970911     C                   EXSR      DESFIL
033900970911     C                   MOVEL     WDFIL         DEDATP
034000970911     C                   Z-ADD     ATFE(I)       DECATE                         *ECCEZIONE
034100970911     C                   Z-ADD     DECATE        WFIL
034200970911     C                   MOVEL     *BLANKS       WDFIL
034300970911     C                   EXSR      DESFIL
034400970911     C                   MOVEL     WDFIL         DEDATE
034500970911E   3C                   ENDIF
034600970911     C* ECCEZIONE PARTENZA
034700020418      * ho lasciato questi campi ma non vengono stampati
034800020418      * xchè per la partenza non sono previste eccezioni
034900970911IF  3C     PTFE(I)       IFGT      *ZEROS
035000020418     C                   Z-ADD     PTFA(I)       DECPTA            3 0          *ARRIVO
035100970911     C                   Z-ADD     DECPTA        WFIL
035200970911     C                   MOVEL     *BLANKS       WDFIL
035300970911     C                   EXSR      DESFIL
035400020418     C                   MOVEL     WDFIL         DEDPTA           15
035500020418     C                   Z-ADD     PTFE(I)       DECPTE            3 0          *ECCEZIONE
035600970911     C                   Z-ADD     DECPTE        WFIL
035700970911     C                   MOVEL     *BLANKS       WDFIL
035800970911     C                   EXSR      DESFIL
035900020418     C                   MOVEL     WDFIL         DEDPTE           15
036000970911E   3C                   ENDIF
036100970911     C*
036200000428     C* REPERISCO DALL'ESTENSIONE ECCEZIONI TERMINAL X POSTE IL RAGGRUPPAMENTO SMISTAMENTO GABBIE
036300000428     C                   EXSR      RTVRAG
036400000428     C*
036500970911     C                   WRITE     SI20DE                                       *DETTAGLIO
036600970911     C*
036700970911E   2C                   ENDIF
036800970911E   1C                   ENDDO
036900970911     C*
037000970911     C                   ENDSR
037100000428     C*------------------------------------------------------------------------*
037200000428     C* RTVRAG - REPERISCE IL RAGGRUPPAMENTO SMISTAMENTI GABBIE DALL'ESTENSIONE TERMINAL X POSTE
037300000428     C*------------------------------------------------------------------------*
037400000428     C     RTVRAG        BEGSR
037500000428     C*
037600000428     C                   MOVEL     *BLANKS       STPRAG                         *RE-INIZIALIZZO SEMP
037700000428     C                   MOVEL     'A'           KPAEPA
037800000428     C*
037900000428     C     DECFIL        IFGT      *ZEROS
038000000428     C                   Z-ADD     DECFIL        POGFIL
038100000428     C                   Z-ADD     DECFIL        KPATFA
038200000428     C                   Z-ADD     *ZEROS        KPATFP
038300000428     C     KEYPAE        SETLL     AZPAE01L
038400000428     C     KEYPAE        READE     AZPAE01L                               98
038500000428DO  1C     *IN98         DOWEQ     *OFF
038600000428     C                   EXSR      CHKPAE                                       *CHEK RECORD VALIDO
038700000428IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
038800000428     C     PAETFE        ANDEQ     DECATS                                       *RECORD VALIDO
038900000428     C                   MOVE      PAERS1        STPRAG                         *RAGGRUPPAMENTO
039000000428     C                   MOVEL     PAERS2        STPRAG                         *RAGGRUPPAMENTO
039100000428E   2C                   ENDIF
039200000428     C     KEYPAE        READE     AZPAE01L                               98
039300000428E   1C                   ENDDO
039400000428     C                   ENDIF
039500000428     C*
039600000428     C     DECFIL        IFEQ      *ZEROS
039700000428     C                   Z-ADD     DECATE        KPATFA
039800000428     C                   Z-ADD     DECATP        KPATFP
039900000428     C     KEYPAE        SETLL     AZPAE01L
040000000428     C     KEYPAE        READE     AZPAE01L                               98
040100000428DO  1C     *IN98         DOWEQ     *OFF
040200000428     C                   EXSR      CHKPAE                                       *CHEK RECORD VALIDO
040300000428IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
040400000428     C     PAETFE        ANDEQ     POGFIL                                       *RECORD VALIDO
040500000428     C                   MOVE      PAERS1        STPRAG                         *RAGGRUPPAMENTO SMIS
040600000428     C                   MOVEL     PAERS2        STPRAG                         *RAGGRUPPAMENTO SMIS
040700000428E   2C                   ENDIF
040800000428     C     KEYPAE        READE     AZPAE01L                               98
040900000428E   1C                   ENDDO
041000000428     C                   ENDIF
041100000428     C*
041200000428     C                   ENDSR
041300000428     C*------------------------------------------------------------------------*
041400000428     C* CHKPAE - CONTROLLA VALIDITA' RECORD ESTENSIONE -AZPAE-
041500000428     C*------------------------------------------------------------------------*
041600000428     C     CHKPAE        BEGSR
041700000428     C*
041800000428     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
041900000428     C*
042000000428     C* RECORD ANNULLATO, ESCLUDE
042100000428IF  1C     PAEATB        IFNE      *BLANKS
042200000428     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042300000428E   1C                   ENDIF
042400000428     C*
042500000428     C* RECORD NON IN DECORRENZA, ESCLUDE
042600000428IF  1C     PARDAT        IFGE      PAEDDE
042700000428     C     PARDAT        ANDLE     PAEDSC
042800000428X   1C                   ELSE
042900000428     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043000000428E   1C                   ENDIF
043100000428     C*
043200000428     C                   ENDSR
043300970911     C*------------------------------------------------------------------------*
043400970911     C* DESFIL - DECODIFICA LA FILIALE
043500970911     C*------------------------------------------------------------------------*
043600970911     C     DESFIL        BEGSR
043700970911     C*
043800970911     C     *LIKE         DEFINE    ORGFIL        WFIL
043900970911     C     *LIKE         DEFINE    ORGDES        WDFIL
044000970911     C*
044100020531     C**!!!              SETOFF                                           98
044200020531     C**!!!              Z-ADD     1             J
044300020531     C**!!!WFIL          LOOKUP    FIL(J)                                 98
044400020531IF  1C**!!!*IN98         IFEQ      *ON                                          *TROVATO
044500020531     C**!!!              MOVEL     DFIL(J)       WDFIL
044600020531X   1C**!!!              ELSE                                                   *NON TROVATO
044700020531     C**!!!              MOVEL     *ALL'*'       WDFIL
044800020531E   1C**!!!              ENDIF
044900020531
045000020531     c     wfil          chain     azorg01l
045100020531     c                   if        %found(azorg01l) and orgfva = *blanks
045200020531     c                   movel     orgdes        wdfil
045300020531     c                   else
045400020531     c                   movel     *all'*'       wdfil
045500020531     c                   endif
045600970911     C*
045700970911     C                   ENDSR
045800970911     C*------------------------------------------------------------------------*
045900970911     C* STATES - STAMPA TESTATA
046000970911     C*------------------------------------------------------------------------*
046100970911     C     STATES        BEGSR
046200970911     C*
046300970911IF  1C     *IN01         IFEQ      *ON                                          *SE FINE PAGINA
046400970911     C                   WRITE     SI20TE                                       *TESTATA
046500970911     C                   WRITE     SI20IN                                       *INTESTAZIONE
046600970911     C                   SETOFF                                       01
046700970911E   1C                   ENDIF
046800970911     C*
046900970911     C                   ENDSR
047000970911     C*------------------------------------------------------------------------*
047100970911     C* STAPAR - STAMPA I PARAEMTRI DI LANCIO
047200970911     C*------------------------------------------------------------------------*
047300970911     C     STAPAR        BEGSR
047400970911     C*
047500970911     C* DECODIFICHE
047600970911     C                   Z-ADD     PARDAT        G08INV
047700970911     C                   Z-ADD     *ZEROS        G08DAT
047800970911     C                   MOVEL     '3'           G08ERR
047900970911     C                   CALL      'XSRDA8'
048000970911     C                   PARM                    WLBDA8
048100970911     C                   Z-ADD     G08DAT        PACDAT
048200970911     C*
048300970911     C                   WRITE     SI20TE                                       *TESTATA
048400970911     C                   WRITE     SI20PA                                       *PARAMETRI
048500970911     C*
048600970911     C                   ENDSR
048700970911     C*------------------------------------------------------------------------*
048800970911     C* CARORG - CARICA L'ORGANIGRAMMA
048900970911     C*------------------------------------------------------------------------*
049000020531     C**!!!CARORG        BEGSR
049100970911     C*
049200020531     C**!!!              Z-ADD     0             I                 5 0          *INDICE
049300020531     C**!!!*LOVAL        SETLL     AZORG05L
049400020531     C**!!!              READ      AZORG05L                               99
049500020531DO  1C**!!!*IN99         DOWEQ     *OFF
049600020531IF  2C**!!!ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
049700020531IF  3C**!!!ORGFAG        IFNE      'V'                                          *NO "VARIE"
049800020531     C**!!!              ADD       1             I
049900020531     C**!!!              Z-ADD     ORGFIL        FIL(I)                         *CODICE
050000020531     C**!!!              MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE
050100020531E   3C**!!!              ENDIF
050200020531E   2C**!!!              ENDIF
050300020531     C**!!!              READ      AZORG05L                               99
050400020531E   1C**!!!              ENDDO
050500970911     C*
050600020531     C**!!!              ENDSR
050700970911     C*------------------------------------------------------------------------*
050800970911     C* *INZSR - OPERAZIONI INIZIALI
050900970911     C*------------------------------------------------------------------------*
051000970911     C     *INZSR        BEGSR
051100970911     C*
051200970911     C* RICEVIMENTO PARAMETRI
051300970911     C     *ENTRY        PLIST
051400970911     C                   PARM                    KPJBA
051500970911     C                   MOVEL     KPJBU         PARAM
051600970911     C*
051700970911     C* VARIABILI RIFERITE AL DATABASE
051800970911     C     *LIKE         DEFINE    CAEEPA        KCAEPA                         *AZCAE00F
051900970911     C     *LIKE         DEFINE    CAETFP        KCATFP
052000970911     C     *LIKE         DEFINE    CAETFA        KCATFA
052100970911     C     *LIKE         DEFINE    ORGDES        DEPDES                          *DEPOSITI
052200970911     C     *LIKE         DEFINE    ORGFIL        DEPFIL
052300971128     C     *LIKE         DEFINE    ORGFIL        DEPFEL
052400000428     C     *LIKE         DEFINE    PAEEPA        KPAEPA                         *AZPAE00F
052500000428     C     *LIKE         DEFINE    PAETFP        KPATFP
052600000428     C     *LIKE         DEFINE    PAETFA        KPATFA
052700000428     C     *LIKE         DEFINE    DECFIL        POGFIL                         *CAMPO PRTF
052800970911     C*
052900970911     C* CHIAVI DI LETTURA
053000970911     C     KEYCAE        KLIST                                                  *AZCAE01L -PARZIALE-
053100970911     C                   KFLD                    KCAEPA                          -PARTENZA/ARRIVO
053200970911     C                   KFLD                    KCATFP                          -TERMINAL PARTENZA
053300970911     C     KE2CAE        KLIST                                                  *AZCAE02L -PARZIALE-
053400970911     C                   KFLD                    KCAEPA                          -PARTENZA/ARRIVO
053500970911     C                   KFLD                    KCATFA                          -TERMINAL ARRIVO
053600000428     C     KEYPAE        KLIST                                                  *AZPAE01L -PARZIALE-
053700000428     C                   KFLD                    KPAEPA                          -PARTENZA/ARRIVO
053800000428     C                   KFLD                    KPATFP                          -TERMINAL PARTENZA
053900000428     C                   KFLD                    KPATFA                          -TERMINAL ARRIVO
054000970911     C*
054100970911     C* VARIABILI DI CONTROLLO
054200970911     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
054300970911     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
054400970911     C                   MOVEL     '1'           *IN01                          *OVERFLOW
054500971128     C*
054600971128     C* CALCOLA LA DATA CORRENTE
054700971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
054800971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
054900971128     C                   Z-ADD     WN8           G08DAT
055000971128     C                   Z-ADD     *ZEROS        G08INV
055100971128     C                   MOVEL     '0'           G08ERR
055200971128     C                   CALL      'XSRDA8'
055300971128     C                   PARM                    WLBDA8
055400971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
055500970911     C*
055600970911     C* CARICA L'ORGANIGRAMMA
055700020531     C**!!!              EXSR      CARORG
055800020531
055900020531      * carico tutti i terminal
056000020531     c                   clear                   trul26ds
056100020531     c                   eval      i26tpt = 'P'
056200020531     c                   eval      i26tla = 'L'
056300020531     c                   call      'TRUL26R'
056400020531     c                   parm                    trul26ds
056500970911     C*
056600970911     C                   ENDSR
