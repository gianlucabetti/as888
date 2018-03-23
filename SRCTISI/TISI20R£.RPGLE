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
002300970911     FTISI20P   O    E             PRINTER OFLIND(*IN01)
002400970911     D*------------------------------------------------------------------------*
002500970911     D* SCHIERE
002600970911     D*------------------------------------------------------------------------*
002700020531
002800970911     D*-------------------
002900970911     D* ECCEZIONI
003000970911     D*-------------------
003100060706     D PTFA            S              3  0 DIM(200)                             *ARRIVO
003200060706     D PTFE            S              3  0 DIM(200)                             *STANDARD/ECCEZIONE
003300060706     D ATFP            S              3  0 DIM(200)                             *PARTENZA
003400060706     D ATFE            S              3  0 DIM(200)                             *STANDARD/ECCEZIONE
003500060706     d stfp            s              3  0 dim(200)                             *partenza
003600060706     d stfe            s              3  0 dim(200)                             *standard/eccesione
003700060706     d OtfA            s              3  0 dim(200)                             *partenza
003800060706     d Otfe            s              3  0 dim(200)                             *standard/eccesione
003900970911     D*------------------------------------------------------------------------*
004000970911     D* INPUT
004100970911     D*------------------------------------------------------------------------*
004200020531
004300020531     d i               s              3  0
004400020531     d ii              s              3  0
004500020531
004600970911     D*-------------------
004700970911     D* DS PARAMETRI DI LANCIO PGM
004800970911     D*-------------------
004900970911     D PARAM           DS
005000970911     D  PARDAT                 1      8  0
005100970911     D*----------------
005200970911     D* DS CONTROLLO DATE
005300970911     D*----------------
005400970911     D WLBDA8          DS
005500970911     D  G08DAT                 1      8  0
005600970911     D  G08INV                 9     16  0
005700970911     D  G08ERR                17     17
005800970911     D  G08TGI                18     22  0
005900971128     D*------------
006000971128     D* DS REPEIMENTO TERMINALs
006100971128     D*------------
006200971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006300970911     D*-------------------
006400970911     D* ARCHITETTURA
006500970911     D*-------------------
006600970911     D KPJBA         E DS
006700020531
006800020531     d trul26ds      e ds
006900020531     d  skpo                  11    760  0 dim(250)
007000020603     d  skpot                761   1510  0 dim(250)
007100970911     C*------------------------------------------------------------------------*
007200970911     C* MAIN LINE
007300970911     C*------------------------------------------------------------------------*
007400970911     C*
007500970911     C* STAMPA I PARAMETRI DI LANCIO
007600970911     C                   EXSR      STAPAR
007700020531
007800020531      * Leggo la schiera dei terminal caricati
007900020531     c                   do        250           ii
008000020531     c                   if        skpo(ii) = *zeros
008100020531     c                   leave
008200020531     c                   endif
008300020531      * controllo se p.o. è ok
008400020531     c     skpo(ii)      chain     azorg01l
008500020531     c                   if        not%found(azorg01l) or orgfva <> *blanks
008600020531     c                             or orgfag = 'V'
008700020531     c                   iter
008800020531     c                   endif
008900970911     C*
009000970911     C* OPERAZIONI PER NUOVA DESCRIZIONE FILIALE
009100970911     C                   EXSR      NEWDES
009200970911     C*
009300970911     C* MEMCAE - MEMORIZZA I TERMINAL STANDARD E ECCEZIONALI DELLA FILIALE
009400970911     C                   EXSR      MEMCAE
009500970911     C*
009600970911     C* STAMPA DATI FILIALE
009700970911     C                   EXSR      STAFIL
009800970911E   1C                   ENDDO                                                  *FINE FILE
009900970911     C*
010000970911     C* STAMPA RIGA DI FINE STAMPA
010100970911     C                   WRITE     SI20FI
010200971128     C*
010300971128     C* OPERAZIONI FINALI
010400971128     C                   EXSR      FINSR
010500970911     C*
010600970911     C                   SETON                                        LR
010700971128     C*------------------------------------------------------------------------*
010800971128     C* FINSR - OPERAZIONI FINALI
010900971128     C*------------------------------------------------------------------------*
011000971128     C     FINSR         BEGSR
011100971128     C*
011200971128     C                   CLEAR                   DSLV55
011300971128     C                   MOVEL     'C'           D55TLA
011400971128     C                   CALL      'FNLV55R'
011500971128     C                   PARM                    DSLV55
011600971128     C*
011700971128     C                   ENDSR
011800970911     C*------------------------------------------------------------------------*
011900970911     C* NEWDES - OPERAZIONI PER NUOVA DESCRIZIONE FILIALE
012000970911     C*------------------------------------------------------------------------*
012100970911     C     NEWDES        BEGSR
012200970911     C*
012300001211     C* MEMORIZZA ALTRI DATI DI USO FUTURO (CODICE FILIALE)
012400001211     C                   Z-ADD     ORGFIL        DEPFIL
012500001211     C*
012600970911     C* MEMORIZZA LA NUOVA DESCRIZIONE FILIALE IN UN DEPOSITO
012700970911     C                   MOVEL     ORGDES        DEPDES
012800970911     C*
012900970911     C* AZZERA LE VARIABILI DI MEMORIZZAZIONE
013000970911     C                   CLEAR                   PTFA                            *ARRIVO
013100970911     C                   CLEAR                   PTFE                            *STANDARD/ECCEZIONE
013200970911     C                   CLEAR                   ATFP                            *PARTENZA
013300970911     C                   CLEAR                   ATFE                            *STANDARD/ECCEZIONE
013400020418     c                   clear                   stfp                            *partenza
013500020418     c                   clear                   stfe                            *standard/eccezione
013600060706     c                   clear                   OtfA                            *partenza
013700060706     c                   clear                   Otfe                            *standard/eccezione
013800970911     C*
013900970911     C                   ENDSR
014000970911     C*------------------------------------------------------------------------*
014100970911     C* MEMCAE - MEMORIZZA I TERMINAL STANDARD E ECCEZIONALI DELLA FILIALE
014200970911     C*------------------------------------------------------------------------*
014300970911     C     MEMCAE        BEGSR
014400970911     C*---
014500970911     C* P A R T E N Z A
014600970911     C*---
014700970911     C* CARICA I TERMINAL PARTENZA
014800970911     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
014900970911     C                   MOVEL     'P'           KCAEPA
015000970911     C                   Z-ADD     ORGFIL        KCATFP
015100970911     C     KEYCAE        SETLL     AZCAE01L
015200970911     C     KEYCAE        READE     AZCAE01L                               98
015300970911DO  1C     *IN98         DOWEQ     *OFF
015400970911     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
015500970911IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
015600970911     C                   ADD       1             I
015700970911     C                   Z-ADD     CAETFA        PTFA(I)                        *ARRIVO
015800970911     C                   Z-ADD     CAETFE        PTFE(I)                        *STANDAR/ECCEZIONE
015900970911E   2C                   ENDIF
016000970911     C     KEYCAE        READE     AZCAE01L                               98
016100970911E   1C                   ENDDO
016200970911     C*
016300970911     C* SPOSTA IL TERMINAL STANDARD PARTENZA IN 1^ POSIZIONE
016400970911DO  1C     1             DO        I             J                 5 0
016500970911IF  2C     PTFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
016600970911IF  3C     PTFA(J)       IFEQ      *ZEROS                                       *STANDARD PARTENZA
016700970911     C                   Z-ADD     PTFA(J)       PTFA(1)                        *SPOSTA IN 1^POS.
016800970911     C                   Z-ADD     PTFE(J)       PTFE(1)
016900970911     C                   Z-ADD     *ZEROS        PTFA(J)                        *CANCELLA ELEMENTO
017000970911     C                   Z-ADD     *ZEROS        PTFE(J)
017100970911     C                   LEAVE
017200970911E   3C                   ENDIF
017300970911E   2C                   ENDIF
017400970911E   1C                   ENDDO
017500970911     C*---
017600970911     C* A R R I V O
017700970911     C*---
017800970911     C* CARICA I TERMINAL ARRIVO
017900970911     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
018000970911     C                   MOVEL     'A'           KCAEPA
018100970911     C                   Z-ADD     ORGFIL        KCATFA
018200970911     C     KE2CAE        SETLL     AZCAE02L
018300970911     C     KE2CAE        READE     AZCAE02L                               98
018400970911DO  1C     *IN98         DOWEQ     *OFF
018500970911     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
018600970911IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
018700970911     C                   ADD       1             I
018800970911     C                   Z-ADD     CAETFP        ATFP(I)                        *PARTENZA
018900970911     C                   Z-ADD     CAETFE        ATFE(I)                        *STANDAR/ECCEZIONE
019000970911E   2C                   ENDIF
019100970911     C     KE2CAE        READE     AZCAE02L                               98
019200970911E   1C                   ENDDO
019300970911     C*
019400970911     C* SPOSTA IL TERMINAL STANDARD ARRIVO IN 1^ POSIZIONE
019500970911DO  1C     1             DO        I             J
019600970911IF  2C     ATFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
019700970911IF  3C     ATFP(J)       IFEQ      *ZEROS                                       *STANDARD ARRIVO
019800970911     C                   Z-ADD     ATFP(J)       ATFP(1)                        *SPOSTA IN 1^POS.
019900970911     C                   Z-ADD     ATFE(J)       ATFE(1)
020000970911     C                   Z-ADD     *ZEROS        ATFP(J)                        *CANCELLA ELEMENTO
020100970911     C                   Z-ADD     *ZEROS        ATFE(J)
020200970911     C                   LEAVE
020300970911E   3C                   ENDIF
020400970911E   2C                   ENDIF
020500970911E   1C                   ENDDO
020600020418      *---
020700020418      * Tipo terminal "6"
020800020418      *---
020900020418      * Carico i terminal tipo "6"
021000020418     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
021100020418     C                   MOVEL     '6'           KCAEPA
021200020418     C                   Z-ADD     ORGFIL        KCATFA
021300020418     C     KE2CAE        SETLL     AZCAE02L
021400020418     C     KE2CAE        READE     AZCAE02L                               98
021500020418DO  1C     *IN98         DOWEQ     *OFF
021600020418     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
021700020418IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
021800020418     C                   ADD       1             I
021900020418     C                   Z-ADD     CAETFP        STFP(I)                        *PARTENZA
022000020418     C                   Z-ADD     CAETFE        STFE(I)                        *STANDARD/ECCEZIONE
022100020418E   2C                   ENDIF
022200020418     C     KE2CAE        READE     AZCAE02L                               98
022300020418E   1C                   ENDDO
022400020418     C*
022500020418     C* SPOSTA IL TERMINAL TIPO "6" IN 1^ POSIZIONE
022600020418DO  1C     1             DO        I             J
022700020418IF  2C     STFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
022800020418IF  3C     STFP(J)       IFEQ      *ZEROS                                       *STANDARD
022900020418     C                   Z-ADD     STFP(J)       STFP(1)                        *SPOSTA IN 1^POS.
023000020418     C                   Z-ADD     STFE(J)       STFE(1)
023100020418     C                   Z-ADD     *ZEROS        STFP(J)                        *CANCELLA ELEMENTO
023200020418     C                   Z-ADD     *ZEROS        STFE(J)
023300020418     C                   LEAVE
023400020418E   3C                   ENDIF
023500020418E   2C                   ENDIF
023600020418E   1C                   ENDDO
023700060706     C*---
023800060706     C* OTTICO CARICO ECCEZIONE SE C'E'
023900060706     C*---
024000060706     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
024100060706     C                   MOVEL     'O'           KCAEPA
024200060706     C                   Z-ADD     ORGFIL        KCATFP
024300060706     C     KEYCAE        SETLL     AZCAE01L
024400060706     C     KEYCAE        READE     AZCAE01L                               98
024500060706DO  1C     *IN98         DOWEQ     *OFF
024600060706     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
024700060706IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
024800060706     C                   ADD       1             I
024900060706     C                   Z-ADD     CAETFA        OTFA(I)                        *ARRIVO
025000060706     C                   Z-ADD     CAETFE        OTFE(I)                        *STANDAR/ECCEZIONE
025100060706E   2C                   ENDIF
025200060706     C     KEYCAE        READE     AZCAE01L                               98
025300060706E   1C                   ENDDO
025400060706     C*
025500060706     C* SPOSTA IL TERMINAL STANDARD PARTENZA IN 1^ POSIZIONE
025600060706DO  1C     1             DO        I             J                 5 0
025700060706IF  2C     OTFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
025800060706IF  3C     OTFA(J)       IFEQ      *ZEROS                                       *STANDARD PARTENZA
025900060706     C                   Z-ADD     OTFA(J)       OTFA(1)                        *SPOSTA IN 1^POS.
026000060706     C                   Z-ADD     OTFE(J)       OTFE(1)
026100060706     C                   Z-ADD     *ZEROS        OTFA(J)                        *CANCELLA ELEMENTO
026200060706     C                   Z-ADD     *ZEROS        OTFE(J)
026300060706     C                   LEAVE
026400060706E   3C                   ENDIF
026500060706E   2C                   ENDIF
026600060706E   1C                   ENDDO
026700970911     C*
026800970911     C                   ENDSR
026900970911     C*------------------------------------------------------------------------*
027000970911     C* CHKCAE - CONTROLLA VALIDITA' RECORD -AZCAE-
027100970911     C*------------------------------------------------------------------------*
027200970911     C     CHKCAE        BEGSR
027300970911     C*
027400970911     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
027500970911     C*
027600970911     C* RECORD ANNULLATO, ESCLUDE
027700970911IF  1C     CAEATB        IFNE      *BLANKS
027800970911     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027900970911E   1C                   ENDIF
028000970911     C*
028100970911     C* RECORD NON IN DECORRENZA, ESCLUDE
028200970911IF  1C     PARDAT        IFGE      CAEDDE
028300970911     C     PARDAT        ANDLE     CAEDSC
028400970911X   1C                   ELSE
028500970911     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028600970911E   1C                   ENDIF
028700970911     C*
028800970911     C                   ENDSR
028900970911     C*------------------------------------------------------------------------*
029000000428     C* STAFIL - STAMPA DATI DI FILIALE
029100970911     C*------------------------------------------------------------------------*
029200970911     C     STAFIL        BEGSR
029300970911     C*
029400970911     C* SE FINE PAGINA, STAMPA TESTATA
029500970911     C                   EXSR      STATES
029600970911     C*
029700970911     C* STAMPA I TERMINAL STANDARD DELLA FILIALE
029800970911     C                   CLEAR                   SI20DE                         *PULISCE FORMATO
029900970911     C                   Z-ADD     DEPFIL        DECFIL                         *FILIALE
030000970911     C                   Z-ADD     DECFIL        WFIL
030100970911     C                   MOVEL     *BLANKS       WDFIL
030200970911     C                   EXSR      DESFIL
030300970911     C                   MOVEL     WDFIL         DEDFIL
030400060706     C* TERMINAL DI ARRIVO STD
030500970911     C                   Z-ADD     ATFE(1)       DECATS                         TERMINAL ARRIVO STD
030600970911     C                   Z-ADD     DECATS        WFIL
030700970911     C                   MOVEL     *BLANKS       WDFIL
030800970911     C                   EXSR      DESFIL
030900970911     C                   MOVEL     WDFIL         DEDATS
031000060706     C* TERMINAL  DI PARTENZA STD
031100970911     C                   Z-ADD     PTFE(1)       DECPTS                         TERMINAL PARTENZA ST
031200970911     C                   Z-ADD     DECPTS        WFIL
031300970911     C                   MOVEL     *BLANKS       WDFIL
031400970911     C                   EXSR      DESFIL
031500970911     C                   MOVEL     WDFIL         DEDPTS
031600060706     C* OTTICO ECCEZIONE (CE NE È SOLO UNO)
031700060706     C                   Z-ADD     OTFE(1)       DECOTT                         TERMINAL PARTENZA ST
031800060706     C                   Z-ADD     DECott        WFIL
031900060706     C                   MOVEL     *BLANKS       WDFIL
032000060706     C                   EXSR      DESFIL
032100060706     C                   MOVEL     WDFIL         DEDOTT
032200060706     C* P.O. £6
032300020418     C                   Z-ADD     STFE(1)       DECTE6                         TERMINAL "6" STD
032400020418     C                   Z-ADD     DECTE6        WFIL
032500020418     C                   MOVEL     *BLANKS       WDFIL
032600020418     C                   EXSR      DESFIL
032700020418     C                   MOVEL     WDFIL         DEDTE6
032800980602     C*
032900980602     C* ESCLUSIONE DEI PUNTI OPERATIVI SENZA TERMINAL ARRIVO E PARTENZA
033000980602IF  1C     PTFE(1)       IFGT      *ZEROS
033100980602     C     ATFE(1)       ORGT      *ZEROS
033200020418     C     STFE(1)       ORGT      *ZEROS
033300000428     C*
033400970911     C                   WRITE     SI20DE                                       *DETTAGLIO
033500980602E   1C                   ENDIF
033600970911     C*
033700970911     C* LEGGE TUTTI I TERMINAL PARTENZA E ARRIVO MEMORIZZATI
033800060706DO  1C     2             DO        200           I
033900970911IF  2C     PTFE(I)       IFGT      *ZEROS
034000970911     C     ATFE(I)       ORGT      *ZEROS
034100020418     C     STFE(I)       ORGT      *ZEROS
034200970911     C                   EXSR      STATES                                       *STAMPA TESTATA
034300970911     C                   CLEAR                   SI20DE                         *PULISCE FORMATO
034400970911     C* ECCEZIONE ARRIVO
034500970911IF  3C     ATFE(I)       IFGT      *ZEROS
034600970911     C                   Z-ADD     ATFP(I)       DECATP                         *PARTENZA
034700970911     C                   Z-ADD     DECATP        WFIL
034800970911     C                   MOVEL     *BLANKS       WDFIL
034900970911     C                   EXSR      DESFIL
035000970911     C                   MOVEL     WDFIL         DEDATP
035100970911     C                   Z-ADD     ATFE(I)       DECATE                         *ECCEZIONE
035200970911     C                   Z-ADD     DECATE        WFIL
035300970911     C                   MOVEL     *BLANKS       WDFIL
035400970911     C                   EXSR      DESFIL
035500970911     C                   MOVEL     WDFIL         DEDATE
035600970911E   3C                   ENDIF
035700970911     C* ECCEZIONE PARTENZA
035800020418      * ho lasciato questi campi ma non vengono stampati
035900020418      * xchè per la partenza non sono previste eccezioni
036000970911IF  3C     PTFE(I)       IFGT      *ZEROS
036100020418     C                   Z-ADD     PTFA(I)       DECPTA            3 0          *ARRIVO
036200970911     C                   Z-ADD     DECPTA        WFIL
036300970911     C                   MOVEL     *BLANKS       WDFIL
036400970911     C                   EXSR      DESFIL
036500020418     C                   MOVEL     WDFIL         DEDPTA           15
036600020418     C                   Z-ADD     PTFE(I)       DECPTE            3 0          *ECCEZIONE
036700970911     C                   Z-ADD     DECPTE        WFIL
036800970911     C                   MOVEL     *BLANKS       WDFIL
036900970911     C                   EXSR      DESFIL
037000020418     C                   MOVEL     WDFIL         DEDPTE           15
037100970911E   3C                   ENDIF
037200970911     C*
037300000428     C*
037400970911     C                   WRITE     SI20DE                                       *DETTAGLIO
037500970911     C*
037600970911E   2C                   ENDIF
037700970911E   1C                   ENDDO
037800970911     C*
037900970911     C                   ENDSR
038000970911     C*------------------------------------------------------------------------*
038100970911     C* DESFIL - DECODIFICA LA FILIALE
038200970911     C*------------------------------------------------------------------------*
038300970911     C     DESFIL        BEGSR
038400970911     C*
038500970911     C     *LIKE         DEFINE    ORGFIL        WFIL
038600970911     C     *LIKE         DEFINE    ORGDES        WDFIL
038700970911     C*
038800020531     C**!!!              SETOFF                                           98
038900020531     C**!!!              Z-ADD     1             J
039000020531     C**!!!WFIL          LOOKUP    FIL(J)                                 98
039100020531IF  1C**!!!*IN98         IFEQ      *ON                                          *TROVATO
039200020531     C**!!!              MOVEL     DFIL(J)       WDFIL
039300020531X   1C**!!!              ELSE                                                   *NON TROVATO
039400020531     C**!!!              MOVEL     *ALL'*'       WDFIL
039500020531E   1C**!!!              ENDIF
039600020531
039700020531     c     wfil          chain     azorg01l
039800020531     c                   if        %found(azorg01l) and orgfva = *blanks
039900020531     c                   movel     orgdes        wdfil
040000020531     c                   else
040100020531     c                   movel     *all'*'       wdfil
040200020531     c                   endif
040300970911     C*
040400970911     C                   ENDSR
040500970911     C*------------------------------------------------------------------------*
040600970911     C* STATES - STAMPA TESTATA
040700970911     C*------------------------------------------------------------------------*
040800970911     C     STATES        BEGSR
040900970911     C*
041000970911IF  1C     *IN01         IFEQ      *ON                                          *SE FINE PAGINA
041100970911     C                   WRITE     SI20TE                                       *TESTATA
041200970911     C                   WRITE     SI20IN                                       *INTESTAZIONE
041300970911     C                   SETOFF                                       01
041400970911E   1C                   ENDIF
041500970911     C*
041600970911     C                   ENDSR
041700970911     C*------------------------------------------------------------------------*
041800970911     C* STAPAR - STAMPA I PARAEMTRI DI LANCIO
041900970911     C*------------------------------------------------------------------------*
042000970911     C     STAPAR        BEGSR
042100970911     C*
042200970911     C* DECODIFICHE
042300970911     C                   Z-ADD     PARDAT        G08INV
042400970911     C                   Z-ADD     *ZEROS        G08DAT
042500970911     C                   MOVEL     '3'           G08ERR
042600970911     C                   CALL      'XSRDA8'
042700970911     C                   PARM                    WLBDA8
042800970911     C                   Z-ADD     G08DAT        PACDAT
042900970911     C*
043000970911     C                   WRITE     SI20TE                                       *TESTATA
043100970911     C                   WRITE     SI20PA                                       *PARAMETRI
043200970911     C*
043300970911     C                   ENDSR
043400970911     C*------------------------------------------------------------------------*
043500970911     C* CARORG - CARICA L'ORGANIGRAMMA
043600970911     C*------------------------------------------------------------------------*
043700020531     C**!!!CARORG        BEGSR
043800970911     C*
043900020531     C**!!!              Z-ADD     0             I                 5 0          *INDICE
044000020531     C**!!!*LOVAL        SETLL     AZORG05L
044100020531     C**!!!              READ      AZORG05L                               99
044200020531DO  1C**!!!*IN99         DOWEQ     *OFF
044300020531IF  2C**!!!ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
044400020531IF  3C**!!!ORGFAG        IFNE      'V'                                          *NO "VARIE"
044500020531     C**!!!              ADD       1             I
044600020531     C**!!!              Z-ADD     ORGFIL        FIL(I)                         *CODICE
044700020531     C**!!!              MOVEL     ORGDES        DFIL(I)                        *DESCRIZIONE
044800020531E   3C**!!!              ENDIF
044900020531E   2C**!!!              ENDIF
045000020531     C**!!!              READ      AZORG05L                               99
045100020531E   1C**!!!              ENDDO
045200970911     C*
045300020531     C**!!!              ENDSR
045400970911     C*------------------------------------------------------------------------*
045500970911     C* *INZSR - OPERAZIONI INIZIALI
045600970911     C*------------------------------------------------------------------------*
045700970911     C     *INZSR        BEGSR
045800970911     C*
045900970911     C* RICEVIMENTO PARAMETRI
046000970911     C     *ENTRY        PLIST
046100970911     C                   PARM                    KPJBA
046200970911     C                   MOVEL     KPJBU         PARAM
046300970911     C*
046400970911     C* VARIABILI RIFERITE AL DATABASE
046500970911     C     *LIKE         DEFINE    CAEEPA        KCAEPA                         *AZCAE00F
046600970911     C     *LIKE         DEFINE    CAETFP        KCATFP
046700970911     C     *LIKE         DEFINE    CAETFA        KCATFA
046800970911     C     *LIKE         DEFINE    ORGDES        DEPDES                          *DEPOSITI
046900970911     C     *LIKE         DEFINE    ORGFIL        DEPFIL
047000971128     C     *LIKE         DEFINE    ORGFIL        DEPFEL
047100970911     C*
047200970911     C* CHIAVI DI LETTURA
047300970911     C     KEYCAE        KLIST                                                  *AZCAE01L -PARZIALE-
047400970911     C                   KFLD                    KCAEPA                          -PARTENZA/ARRIVO
047500970911     C                   KFLD                    KCATFP                          -TERMINAL PARTENZA
047600970911     C     KE2CAE        KLIST                                                  *AZCAE02L -PARZIALE-
047700970911     C                   KFLD                    KCAEPA                          -PARTENZA/ARRIVO
047800970911     C                   KFLD                    KCATFA                          -TERMINAL ARRIVO
047900970911     C*
048000970911     C* VARIABILI DI CONTROLLO
048100970911     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
048200970911     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
048300970911     C                   MOVEL     '1'           *IN01                          *OVERFLOW
048400971128     C*
048500971128     C* CALCOLA LA DATA CORRENTE
048600971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
048700971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
048800971128     C                   Z-ADD     WN8           G08DAT
048900971128     C                   Z-ADD     *ZEROS        G08INV
049000971128     C                   MOVEL     '0'           G08ERR
049100971128     C                   CALL      'XSRDA8'
049200971128     C                   PARM                    WLBDA8
049300971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
049400970911     C*
049500970911     C* CARICA L'ORGANIGRAMMA
049600020531     C**!!!              EXSR      CARORG
049700020531
049800020531      * carico tutti i terminal
049900020531     c                   clear                   trul26ds
050000020531     c                   eval      i26tpt = 'P'
050100020531     c                   eval      i26tla = 'L'
050200080612     c                   z-add     pardat        i26dta
050300020531     c                   call      'TRUL26R'
050400020531     c                   parm                    trul26ds
050500970911     C*
050600970911     C                   ENDSR
