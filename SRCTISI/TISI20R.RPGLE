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
003900140120     d Ctfp            s              3  0 dim(200)                             *partenza
004000140120     d Ctfe            s              3  0 dim(200)                             *standard/eccesione
004100140120     d jtfp            s              3  0 dim(200)                             *partenza
004200140120     d jtfe            s              3  0 dim(200)                             *standard/eccesione
004300970911     D*------------------------------------------------------------------------*
004400970911     D* INPUT
004500970911     D*------------------------------------------------------------------------*
004600020531
004700020531     d i               s              3  0
004800020531     d ii              s              3  0
004900020531
005000970911     D*-------------------
005100970911     D* DS PARAMETRI DI LANCIO PGM
005200970911     D*-------------------
005300970911     D PARAM           DS
005400970911     D  PARDAT                 1      8  0
005500970911     D*----------------
005600970911     D* DS CONTROLLO DATE
005700970911     D*----------------
005800970911     D WLBDA8          DS
005900970911     D  G08DAT                 1      8  0
006000970911     D  G08INV                 9     16  0
006100970911     D  G08ERR                17     17
006200970911     D  G08TGI                18     22  0
006300971128     D*------------
006400971128     D* DS REPEIMENTO TERMINALs
006500971128     D*------------
006600971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006700970911     D*-------------------
006800970911     D* ARCHITETTURA
006900970911     D*-------------------
007000970911     D KPJBA         E DS
007100020531
007200020531     d trul26ds      e ds
007300020531     d  skpo                  11    760  0 dim(250)
007400020603     d  skpot                761   1510  0 dim(250)
007500970911     C*------------------------------------------------------------------------*
007600970911     C* MAIN LINE
007700970911     C*------------------------------------------------------------------------*
007800970911     C*
007900970911     C* STAMPA I PARAMETRI DI LANCIO
008000970911     C                   EXSR      STAPAR
008100020531
008200020531      * Leggo la schiera dei terminal caricati
008300020531     c                   do        250           ii
008400020531     c                   if        skpo(ii) = *zeros
008500020531     c                   leave
008600020531     c                   endif
008700020531      * controllo se p.o. è ok
008800020531     c     skpo(ii)      chain     azorg01l
008900020531     c                   if        not%found(azorg01l) or orgfva <> *blanks
009000020531     c                             or orgfag = 'V'
009100020531     c                   iter
009200020531     c                   endif
009300970911     C*
009400970911     C* OPERAZIONI PER NUOVA DESCRIZIONE FILIALE
009500970911     C                   EXSR      NEWDES
009600970911     C*
009700970911     C* MEMCAE - MEMORIZZA I TERMINAL STANDARD E ECCEZIONALI DELLA FILIALE
009800970911     C                   EXSR      MEMCAE
009900970911     C*
010000970911     C* STAMPA DATI FILIALE
010100970911     C                   EXSR      STAFIL
010200970911E   1C                   ENDDO                                                  *FINE FILE
010300970911     C*
010400970911     C* STAMPA RIGA DI FINE STAMPA
010500970911     C                   WRITE     SI20FI
010600971128     C*
010700971128     C* OPERAZIONI FINALI
010800971128     C                   EXSR      FINSR
010900970911     C*
011000970911     C                   SETON                                        LR
011100971128     C*------------------------------------------------------------------------*
011200971128     C* FINSR - OPERAZIONI FINALI
011300971128     C*------------------------------------------------------------------------*
011400971128     C     FINSR         BEGSR
011500971128     C*
011600971128     C                   CLEAR                   DSLV55
011700971128     C                   MOVEL     'C'           D55TLA
011800971128     C                   CALL      'FNLV55R'
011900971128     C                   PARM                    DSLV55
012000971128     C*
012100971128     C                   ENDSR
012200970911     C*------------------------------------------------------------------------*
012300970911     C* NEWDES - OPERAZIONI PER NUOVA DESCRIZIONE FILIALE
012400970911     C*------------------------------------------------------------------------*
012500970911     C     NEWDES        BEGSR
012600970911     C*
012700001211     C* MEMORIZZA ALTRI DATI DI USO FUTURO (CODICE FILIALE)
012800001211     C                   Z-ADD     ORGFIL        DEPFIL
012900001211     C*
013000970911     C* MEMORIZZA LA NUOVA DESCRIZIONE FILIALE IN UN DEPOSITO
013100970911     C                   MOVEL     ORGDES        DEPDES
013200970911     C*
013300970911     C* AZZERA LE VARIABILI DI MEMORIZZAZIONE
013400970911     C                   CLEAR                   PTFA                            *ARRIVO
013500970911     C                   CLEAR                   PTFE                            *STANDARD/ECCEZIONE
013600970911     C                   CLEAR                   ATFP                            *PARTENZA
013700970911     C                   CLEAR                   ATFE                            *STANDARD/ECCEZIONE
013800020418     c                   clear                   stfp                            *partenza
013900020418     c                   clear                   stfe                            *standard/eccezione
014000060706     c                   clear                   OtfA                            *partenza
014100060706     c                   clear                   Otfe                            *standard/eccezione
014200140122     c                   clear                   CtfP                            *partenza
014300140122     c                   clear                   Ctfe                            *standard/eccezione
014400140122     c                   clear                   JtfP                            *partenza
014500140122     c                   clear                   Jtfe                            *standard/eccezione
014600970911     C*
014700970911     C                   ENDSR
014800970911     C*------------------------------------------------------------------------*
014900970911     C* MEMCAE - MEMORIZZA I TERMINAL STANDARD E ECCEZIONALI DELLA FILIALE
015000970911     C*------------------------------------------------------------------------*
015100970911     C     MEMCAE        BEGSR
015200970911     C*---
015300970911     C* P A R T E N Z A
015400970911     C*---
015500970911     C* CARICA I TERMINAL PARTENZA
015600970911     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
015700970911     C                   MOVEL     'P'           KCAEPA
015800970911     C                   Z-ADD     ORGFIL        KCATFP
015900970911     C     KEYCAE        SETLL     AZCAE01L
016000970911     C     KEYCAE        READE     AZCAE01L                               98
016100970911DO  1C     *IN98         DOWEQ     *OFF
016200970911     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
016300970911IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
016400970911     C                   ADD       1             I
016500970911     C                   Z-ADD     CAETFA        PTFA(I)                        *ARRIVO
016600970911     C                   Z-ADD     CAETFE        PTFE(I)                        *STANDAR/ECCEZIONE
016700970911E   2C                   ENDIF
016800970911     C     KEYCAE        READE     AZCAE01L                               98
016900970911E   1C                   ENDDO
017000970911     C*
017100970911     C* SPOSTA IL TERMINAL STANDARD PARTENZA IN 1^ POSIZIONE
017200970911DO  1C     1             DO        I             J                 5 0
017300970911IF  2C     PTFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
017400970911IF  3C     PTFA(J)       IFEQ      *ZEROS                                       *STANDARD PARTENZA
017500970911     C                   Z-ADD     PTFA(J)       PTFA(1)                        *SPOSTA IN 1^POS.
017600970911     C                   Z-ADD     PTFE(J)       PTFE(1)
017700970911     C                   Z-ADD     *ZEROS        PTFA(J)                        *CANCELLA ELEMENTO
017800970911     C                   Z-ADD     *ZEROS        PTFE(J)
017900970911     C                   LEAVE
018000970911E   3C                   ENDIF
018100970911E   2C                   ENDIF
018200970911E   1C                   ENDDO
018300970911     C*---
018400970911     C* A R R I V O
018500970911     C*---
018600970911     C* CARICA I TERMINAL ARRIVO
018700970911     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
018800970911     C                   MOVEL     'A'           KCAEPA
018900970911     C                   Z-ADD     ORGFIL        KCATFA
019000970911     C     KE2CAE        SETLL     AZCAE02L
019100970911     C     KE2CAE        READE     AZCAE02L                               98
019200970911DO  1C     *IN98         DOWEQ     *OFF
019300970911     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
019400970911IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
019500970911     C                   ADD       1             I
019600970911     C                   Z-ADD     CAETFP        ATFP(I)                        *PARTENZA
019700970911     C                   Z-ADD     CAETFE        ATFE(I)                        *STANDAR/ECCEZIONE
019800970911E   2C                   ENDIF
019900970911     C     KE2CAE        READE     AZCAE02L                               98
020000970911E   1C                   ENDDO
020100970911     C*
020200970911     C* SPOSTA IL TERMINAL STANDARD ARRIVO IN 1^ POSIZIONE
020300970911DO  1C     1             DO        I             J
020400970911IF  2C     ATFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
020500970911IF  3C     ATFP(J)       IFEQ      *ZEROS                                       *STANDARD ARRIVO
020600970911     C                   Z-ADD     ATFP(J)       ATFP(1)                        *SPOSTA IN 1^POS.
020700970911     C                   Z-ADD     ATFE(J)       ATFE(1)
020800970911     C                   Z-ADD     *ZEROS        ATFP(J)                        *CANCELLA ELEMENTO
020900970911     C                   Z-ADD     *ZEROS        ATFE(J)
021000970911     C                   LEAVE
021100970911E   3C                   ENDIF
021200970911E   2C                   ENDIF
021300970911E   1C                   ENDDO
021400020418      *---
021500020418      * Tipo terminal "6"
021600020418      *---
021700020418      * Carico i terminal tipo "6"
021800020418     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
021900020418     C                   MOVEL     '6'           KCAEPA
022000020418     C                   Z-ADD     ORGFIL        KCATFA
022100020418     C     KE2CAE        SETLL     AZCAE02L
022200020418     C     KE2CAE        READE     AZCAE02L                               98
022300020418DO  1C     *IN98         DOWEQ     *OFF
022400020418     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
022500020418IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
022600020418     C                   ADD       1             I
022700020418     C                   Z-ADD     CAETFP        STFP(I)                        *PARTENZA
022800020418     C                   Z-ADD     CAETFE        STFE(I)                        *STANDARD/ECCEZIONE
022900020418E   2C                   ENDIF
023000020418     C     KE2CAE        READE     AZCAE02L                               98
023100020418E   1C                   ENDDO
023200020418     C*
023300020418     C* SPOSTA IL TERMINAL TIPO "6" IN 1^ POSIZIONE
023400020418DO  1C     1             DO        I             J
023500020418IF  2C     STFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
023600020418IF  3C     STFP(J)       IFEQ      *ZEROS                                       *STANDARD
023700020418     C                   Z-ADD     STFP(J)       STFP(1)                        *SPOSTA IN 1^POS.
023800020418     C                   Z-ADD     STFE(J)       STFE(1)
023900020418     C                   Z-ADD     *ZEROS        STFP(J)                        *CANCELLA ELEMENTO
024000020418     C                   Z-ADD     *ZEROS        STFE(J)
024100020418     C                   LEAVE
024200020418E   3C                   ENDIF
024300020418E   2C                   ENDIF
024400020418E   1C                   ENDDO
024500140120      *---
024600140120      * Carico i terminal tipo "C"
024700140120     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
024800140120     C                   MOVEL     'C'           KCAEPA
024900140120     C                   Z-ADD     ORGFIL        KCATFA
025000140120     C     KE2CAE        SETLL     AZCAE02L
025100140120     C     KE2CAE        READE     AZCAE02L                               98
025200140120DO  1C     *IN98         DOWEQ     *OFF
025300140120     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
025400140120IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
025500140120     C                   ADD       1             I
025600140120     C                   Z-ADD     CAETFP        CTFP(I)                        *PARTENZA
025700140120     C                   Z-ADD     CAETFE        CTFE(I)                        *STANDARD/ECCEZIONE
025800140120E   2C                   ENDIF
025900140120     C     KE2CAE        READE     AZCAE02L                               98
026000140120E   1C                   ENDDO
026100140120     C*
026200140120     C* SPOSTA IL TERMINAL TIPO "C" IN 1^ POSIZIONE
026300140120DO  1C     1             DO        I             J
026400140122IF  2C     cTFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
026500140122IF  3C     cTFP(J)       IFEQ      *ZEROS                                       *STANDARD
026600140122     C                   Z-ADD     cTFP(J)       CTFP(1)                        *SPOSTA IN 1^POS.
026700140122     C                   Z-ADD     cTFE(J)       CTFE(1)
026800140120     C                   Z-ADD     *ZEROS        CTFP(J)                        *CANCELLA ELEMENTO
026900140120     C                   Z-ADD     *ZEROS        CTFE(J)
027000140120     C                   LEAVE
027100140120E   3C                   ENDIF
027200140120E   2C                   ENDIF
027300140120E   1C                   ENDDO
027400140120      *---
027500140120      * Carico i terminal tipo "J"
027600140120     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
027700140120     C                   MOVEL     'J'           KCAEPA
027800140120     C                   Z-ADD     ORGFIL        KCATFA
027900140120     C     KE2CAE        SETLL     AZCAE02L
028000140120     C     KE2CAE        READE     AZCAE02L                               98
028100140120DO  1C     *IN98         DOWEQ     *OFF
028200140120     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
028300140120IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
028400140120     C                   ADD       1             I
028500140120     C                   Z-ADD     CAETFP        JTFP(I)                        *PARTENZA
028600140120     C                   Z-ADD     CAETFE        JTFE(I)                        *STANDARD/ECCEZIONE
028700140120E   2C                   ENDIF
028800140120     C     KE2CAE        READE     AZCAE02L                               98
028900140120E   1C                   ENDDO
029000140120     C*
029100140120     C* SPOSTA IL TERMINAL TIPO "C" IN 1^ POSIZIONE
029200140120DO  1C     1             DO        I             J
029300140122IF  2C     jTFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
029400140122IF  3C     jTFP(J)       IFEQ      *ZEROS                                       *STANDARD
029500140122     C                   Z-ADD     jTFP(J)       jTFP(1)                        *SPOSTA IN 1^POS.
029600140122     C                   Z-ADD     jTFE(J)       jTFE(1)
029700140120     C                   Z-ADD     *ZEROS        jTFP(J)                        *CANCELLA ELEMENTO
029800140120     C                   Z-ADD     *ZEROS        jTFE(J)
029900140120     C                   LEAVE
030000140120E   3C                   ENDIF
030100140120E   2C                   ENDIF
030200140120E   1C                   ENDDO
030300060706     C*---
030400060706     C* OTTICO CARICO ECCEZIONE SE C'E'
030500060706     C*---
030600060706     C                   Z-ADD     1             I                              *PARTE DALLA 2^POS.
030700060706     C                   MOVEL     'O'           KCAEPA
030800060706     C                   Z-ADD     ORGFIL        KCATFP
030900060706     C     KEYCAE        SETLL     AZCAE01L
031000060706     C     KEYCAE        READE     AZCAE01L                               98
031100060706DO  1C     *IN98         DOWEQ     *OFF
031200060706     C                   EXSR      CHKCAE                                       *CHEK RECORD VALIDO
031300060706IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
031400060706     C                   ADD       1             I
031500060706     C                   Z-ADD     CAETFA        OTFA(I)                        *ARRIVO
031600060706     C                   Z-ADD     CAETFE        OTFE(I)                        *STANDAR/ECCEZIONE
031700060706E   2C                   ENDIF
031800060706     C     KEYCAE        READE     AZCAE01L                               98
031900060706E   1C                   ENDDO
032000060706     C*
032100060706     C* SPOSTA IL TERMINAL STANDARD PARTENZA IN 1^ POSIZIONE
032200060706DO  1C     1             DO        I             J                 5 0
032300060706IF  2C     OTFE(J)       IFGT      *ZEROS                                       *SE ELEMENTO PIENO
032400060706IF  3C     OTFA(J)       IFEQ      *ZEROS                                       *STANDARD PARTENZA
032500060706     C                   Z-ADD     OTFA(J)       OTFA(1)                        *SPOSTA IN 1^POS.
032600060706     C                   Z-ADD     OTFE(J)       OTFE(1)
032700060706     C                   Z-ADD     *ZEROS        OTFA(J)                        *CANCELLA ELEMENTO
032800060706     C                   Z-ADD     *ZEROS        OTFE(J)
032900060706     C                   LEAVE
033000060706E   3C                   ENDIF
033100060706E   2C                   ENDIF
033200060706E   1C                   ENDDO
033300970911     C*
033400970911     C                   ENDSR
033500970911     C*------------------------------------------------------------------------*
033600970911     C* CHKCAE - CONTROLLA VALIDITA' RECORD -AZCAE-
033700970911     C*------------------------------------------------------------------------*
033800970911     C     CHKCAE        BEGSR
033900970911     C*
034000970911     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
034100970911     C*
034200970911     C* RECORD ANNULLATO, ESCLUDE
034300970911IF  1C     CAEATB        IFNE      *BLANKS
034400970911     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
034500970911E   1C                   ENDIF
034600970911     C*
034700970911     C* RECORD NON IN DECORRENZA, ESCLUDE
034800970911IF  1C     PARDAT        IFGE      CAEDDE
034900970911     C     PARDAT        ANDLE     CAEDSC
035000970911X   1C                   ELSE
035100970911     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
035200970911E   1C                   ENDIF
035300970911     C*
035400970911     C                   ENDSR
035500970911     C*------------------------------------------------------------------------*
035600000428     C* STAFIL - STAMPA DATI DI FILIALE
035700970911     C*------------------------------------------------------------------------*
035800970911     C     STAFIL        BEGSR
035900970911     C*
036000970911     C* SE FINE PAGINA, STAMPA TESTATA
036100970911     C                   EXSR      STATES
036200970911     C*
036300970911     C* STAMPA I TERMINAL STANDARD DELLA FILIALE
036400970911     C                   CLEAR                   SI20DE                         *PULISCE FORMATO
036500970911     C                   Z-ADD     DEPFIL        DECFIL                         *FILIALE
036600970911     C                   Z-ADD     DECFIL        WFIL
036700970911     C                   MOVEL     *BLANKS       WDFIL
036800970911     C                   EXSR      DESFIL
036900970911     C                   MOVEL     WDFIL         DEDFIL
037000060706     C* TERMINAL DI ARRIVO STD
037100970911     C                   Z-ADD     ATFE(1)       DECATS                         TERMINAL ARRIVO STD
037200970911     C                   Z-ADD     DECATS        WFIL
037300970911     C                   MOVEL     *BLANKS       WDFIL
037400970911     C                   EXSR      DESFIL
037500970911     C                   MOVEL     WDFIL         DEDATS
037600060706     C* TERMINAL  DI PARTENZA STD
037700970911     C                   Z-ADD     PTFE(1)       DECPTS                         TERMINAL PARTENZA ST
037800970911     C                   Z-ADD     DECPTS        WFIL
037900970911     C                   MOVEL     *BLANKS       WDFIL
038000970911     C                   EXSR      DESFIL
038100970911     C                   MOVEL     WDFIL         DEDPTS
038200060706     C* OTTICO ECCEZIONE (CE NE È SOLO UNO)
038300060706     C                   Z-ADD     OTFE(1)       DECOTT                         TERMINAL PARTENZA ST
038400060706     C                   Z-ADD     DECott        WFIL
038500060706     C                   MOVEL     *BLANKS       WDFIL
038600060706     C                   EXSR      DESFIL
038700060706     C                   MOVEL     WDFIL         DEDOTT
038800060706     C* P.O. £6
038900020418     C                   Z-ADD     STFE(1)       DECTE6                         TERMINAL "6" STD
039000020418     C                   Z-ADD     DECTE6        WFIL
039100020418     C                   MOVEL     *BLANKS       WDFIL
039200020418     C                   EXSR      DESFIL
039300020418     C                   MOVEL     WDFIL         DEDTE6
039400140120     C* P.O. COOP
039500140120     C                   Z-ADD     CTFE(1)       DECTEC                         TERMINAL "C" STD
039600140120     C* P.O. £J
039700140120     C                   Z-ADD     JTFE(1)       DECTEJ
039800140120     C                   Z-ADD     DECTEJ        WFIL
039900140120     C                   MOVEL     *BLANKS       WDFIL
040000140120     C                   EXSR      DESFIL
040100140120     C                   MOVEL     WDFIL         DEDTEJ
040200980602     C*
040300980602     C* ESCLUSIONE DEI PUNTI OPERATIVI SENZA TERMINAL ARRIVO E PARTENZA
040400980602IF  1C     PTFE(1)       IFGT      *ZEROS
040500980602     C     ATFE(1)       ORGT      *ZEROS
040600020418     C     STFE(1)       ORGT      *ZEROS
040700140120     C     CTFE(1)       ORGT      *ZEROS
040800140120     C     JTFE(1)       ORGT      *ZEROS
040900000428     C*
041000970911     C                   WRITE     SI20DE                                       *DETTAGLIO
041100980602E   1C                   ENDIF
041200970911     C*
041300970911     C* LEGGE TUTTI I TERMINAL PARTENZA E ARRIVO MEMORIZZATI
041400060706DO  1C     2             DO        200           I
041500970911IF  2C     PTFE(I)       IFGT      *ZEROS
041600970911     C     ATFE(I)       ORGT      *ZEROS
041700020418     C     STFE(I)       ORGT      *ZEROS
041800140120     C     CTFE(I)       ORGT      *ZEROS
041900140120     C     JTFE(I)       ORGT      *ZEROS
042000970911     C                   EXSR      STATES                                       *STAMPA TESTATA
042100970911     C                   CLEAR                   SI20DE                         *PULISCE FORMATO
042200970911     C* ECCEZIONE ARRIVO
042300970911IF  3C     ATFE(I)       IFGT      *ZEROS
042400970911     C                   Z-ADD     ATFP(I)       DECATP                         *PARTENZA
042500970911     C                   Z-ADD     DECATP        WFIL
042600970911     C                   MOVEL     *BLANKS       WDFIL
042700970911     C                   EXSR      DESFIL
042800970911     C                   MOVEL     WDFIL         DEDATP
042900970911     C                   Z-ADD     ATFE(I)       DECATE                         *ECCEZIONE
043000970911     C                   Z-ADD     DECATE        WFIL
043100970911     C                   MOVEL     *BLANKS       WDFIL
043200970911     C                   EXSR      DESFIL
043300970911     C                   MOVEL     WDFIL         DEDATE
043400970911E   3C                   ENDIF
043500970911     C* ECCEZIONE PARTENZA
043600020418      * ho lasciato questi campi ma non vengono stampati
043700020418      * xchè per la partenza non sono previste eccezioni
043800970911IF  3C     PTFE(I)       IFGT      *ZEROS
043900020418     C                   Z-ADD     PTFA(I)       DECPTA            3 0          *ARRIVO
044000970911     C                   Z-ADD     DECPTA        WFIL
044100970911     C                   MOVEL     *BLANKS       WDFIL
044200970911     C                   EXSR      DESFIL
044300020418     C                   MOVEL     WDFIL         DEDPTA           15
044400020418     C                   Z-ADD     PTFE(I)       DECPTE            3 0          *ECCEZIONE
044500970911     C                   Z-ADD     DECPTE        WFIL
044600970911     C                   MOVEL     *BLANKS       WDFIL
044700970911     C                   EXSR      DESFIL
044800020418     C                   MOVEL     WDFIL         DEDPTE           15
044900970911E   3C                   ENDIF
045000970911     C*
045100000428     C*
045200970911     C                   WRITE     SI20DE                                       *DETTAGLIO
045300970911     C*
045400970911E   2C                   ENDIF
045500970911E   1C                   ENDDO
045600970911     C*
045700970911     C                   ENDSR
045800970911     C*------------------------------------------------------------------------*
045900970911     C* DESFIL - DECODIFICA LA FILIALE
046000970911     C*------------------------------------------------------------------------*
046100970911     C     DESFIL        BEGSR
046200970911     C*
046300970911     C     *LIKE         DEFINE    ORGFIL        WFIL
046400970911     C     *LIKE         DEFINE    ORGDES        WDFIL
046500970911     C*
046600020531     c     wfil          chain     azorg01l
046700020531     c                   if        %found(azorg01l) and orgfva = *blanks
046800020531     c                   movel     orgdes        wdfil
046900020531     c                   else
047000020531     c                   movel     *all'*'       wdfil
047100020531     c                   endif
047200970911     C*
047300970911     C                   ENDSR
047400970911     C*------------------------------------------------------------------------*
047500970911     C* STATES - STAMPA TESTATA
047600970911     C*------------------------------------------------------------------------*
047700970911     C     STATES        BEGSR
047800970911     C*
047900970911IF  1C     *IN01         IFEQ      *ON                                          *SE FINE PAGINA
048000970911     C                   WRITE     SI20TE                                       *TESTATA
048100970911     C                   WRITE     SI20IN                                       *INTESTAZIONE
048200970911     C                   SETOFF                                       01
048300970911E   1C                   ENDIF
048400970911     C*
048500970911     C                   ENDSR
048600970911     C*------------------------------------------------------------------------*
048700970911     C* STAPAR - STAMPA I PARAEMTRI DI LANCIO
048800970911     C*------------------------------------------------------------------------*
048900970911     C     STAPAR        BEGSR
049000970911     C*
049100970911     C* DECODIFICHE
049200970911     C                   Z-ADD     PARDAT        G08INV
049300970911     C                   Z-ADD     *ZEROS        G08DAT
049400970911     C                   MOVEL     '3'           G08ERR
049500970911     C                   CALL      'XSRDA8'
049600970911     C                   PARM                    WLBDA8
049700970911     C                   Z-ADD     G08DAT        PACDAT
049800970911     C*
049900970911     C                   WRITE     SI20TE                                       *TESTATA
050000970911     C                   WRITE     SI20PA                                       *PARAMETRI
050100970911     C*
050200970911     C                   ENDSR
050300970911     C*------------------------------------------------------------------------*
050400970911     C* *INZSR - OPERAZIONI INIZIALI
050500970911     C*------------------------------------------------------------------------*
050600970911     C     *INZSR        BEGSR
050700970911     C*
050800970911     C* RICEVIMENTO PARAMETRI
050900970911     C     *ENTRY        PLIST
051000970911     C                   PARM                    KPJBA
051100970911     C                   MOVEL     KPJBU         PARAM
051200970911     C*
051300970911     C* VARIABILI RIFERITE AL DATABASE
051400970911     C     *LIKE         DEFINE    CAEEPA        KCAEPA                         *AZCAE00F
051500970911     C     *LIKE         DEFINE    CAETFP        KCATFP
051600970911     C     *LIKE         DEFINE    CAETFA        KCATFA
051700970911     C     *LIKE         DEFINE    ORGDES        DEPDES                          *DEPOSITI
051800970911     C     *LIKE         DEFINE    ORGFIL        DEPFIL
051900971128     C     *LIKE         DEFINE    ORGFIL        DEPFEL
052000970911     C*
052100970911     C* CHIAVI DI LETTURA
052200970911     C     KEYCAE        KLIST                                                  *AZCAE01L -PARZIALE-
052300970911     C                   KFLD                    KCAEPA                          -PARTENZA/ARRIVO
052400970911     C                   KFLD                    KCATFP                          -TERMINAL PARTENZA
052500970911     C     KE2CAE        KLIST                                                  *AZCAE02L -PARZIALE-
052600970911     C                   KFLD                    KCAEPA                          -PARTENZA/ARRIVO
052700970911     C                   KFLD                    KCATFA                          -TERMINAL ARRIVO
052800970911     C*
052900970911     C* VARIABILI DI CONTROLLO
053000970911     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
053100970911     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
053200970911     C                   MOVEL     '1'           *IN01                          *OVERFLOW
053300971128     C*
053400971128     C* CALCOLA LA DATA CORRENTE
053500971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
053600971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
053700971128     C                   Z-ADD     WN8           G08DAT
053800971128     C                   Z-ADD     *ZEROS        G08INV
053900971128     C                   MOVEL     '0'           G08ERR
054000971128     C                   CALL      'XSRDA8'
054100971128     C                   PARM                    WLBDA8
054200971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
054300020531
054400020531      * carico tutti i terminal
054500020531     c                   clear                   trul26ds
054600020531     c                   eval      i26tpt = 'P'
054700020531     c                   eval      i26tla = 'L'
054800080612     c                   z-add     pardat        i26dta
054900020531     c                   call      'TRUL26R'
055000020531     c                   parm                    trul26ds
055100970911     C*
055200970911     C                   ENDSR
