000100970526     H*------------------------------------------------------------------------*
000200040823     H* Controllo regole clienti/tipi file internet
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800040823     Ftivir02l  if   e           k disk
000900040823     Ftivtf01l  if   e           k disk
001000000626     Ftntbe01l  if   e           k disk
001100040823     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500040823     D*------------
001600040823     D* MESSAGGI
001700040823     D*------------
001800040823     D MSG             S             78    DIM(25) CTDATA PERRCD(1)
001900020218     D*------------
002000020218     D* DS "ITF" - TABELLA SCAMBIO TIPI FILE
002100020218     D*------------
002200020218     D DITF          E DS
002300970526     D*------------
002400970526     D* DS DI PROCEDURA
002500970526     D*------------
002600040823     D DISV          E DS
002700040816     D DIFT          E DS
002800040823     D*------------
002900040823     D* TABELLE
003000040823     D*------------
003100040823     D TIBS02DS      E DS
003200970526     D*------------
003300970526     D* ARCHITETTURA
003400970526     D*------------
003500970526     D KPJBA         E DS
003600040816     D*------------
003700040816     D* VARIABILI D WRK
003800040816     D*------------
003900040823     D skTipF          s              2    dim(500)
004000040816     D skPgt           s             10    dim(500)
004100040816     D skFileT         s             10    dim(500)
004200040816     D j               s              3  0 inz
004300040823     D wVIRPGT         s                   like(VIRPGT)
004400040823     D wVIRFIT         s                   like(VIRFIT)
004500040823     D MESSAGGIO       s            100    inz(*blanks)
004600970526     C*------------------------------------------------------------------------*
004700970526     C* MAIN LINES
004800970526     C*------------------------------------------------------------------------*
004900040823     C*
005000040823     C* ROUTINE D ELABORAZIONE
005100040823     C                   EXSR      CHKVIR
005200040823     C*
005300970526     C                   SETON                                        LR
005400040823     C*------------------------------------------------------------------------*
005500040823     C* CHKVIR - CONTROLLA REGOLE CLIENTI/TIPI FILE
005600040823     C*------------------------------------------------------------------------*
005700040823     C     CHKVIR        BEGSR
005800040823     C*
005900040823     C* Legge tutto il TIVIR
006000040823     C     *LOVAL        SETLL     TIVIR02L
006100040823     C                   READ      TIVIR02L
006200040823     C                   DOW       not %eof(TIVIR02L)
006300040823     C* Inizializzo flag d errore che condizionerà poi la stampa o meno del record corrente
006400040823     C                   MOVEL     '0'           ERRL00            1
006500040823     C* Eseguo routine dei controlli sul record corrente
006600040823     C                   EXSR      DECODI
006700040823     C* Quindi proseguo la lettura del file regole clienti/tipi file
006800040823     C                   READ      TIVIR02L
006900040823     C                   ENDDO
007000040823     C*
007100040823     C                   ENDSR
007200970526     C*------------------------------------------------------------------------*
007300970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
007400970526     C*------------------------------------------------------------------------*
007500970526     C     DECODI        BEGSR
007600001120     C*
007700001120     C*----------------------
007800001120     C* Controllo tipo file
007900001120     C*----------------------
008000040823     C                   EXSR      cnttip
008100040823     C* Se occorso errore listo record corrente in stampa
008200040823     C                   IF        ERRL00 = '1'
008300040823     C                   EXCEPT    DET
008400040823     C                   MOVEL     '0'           ERRL00
008500040823     C                   ENDIF
008600001120     C*----------------------
008700001120     C* Tipo servizio
008800001120     C*----------------------
008900040823     C                   EXSR      cntisv
009000040823     C* Se occorso errore listo record corrente in stampa
009100040823     C                   IF        ERRL00 = '1'
009200040823     C                   EXCEPT    DET
009300040823     C                   MOVEL     '0'           ERRL00
009400040823     C                   ENDIF
009500040319     C* Verifico la compatibilità del servizio rispetto al tipo-file
009600040823     C     VIRTIP        CHAIN     TIVTF01L
009700040319     C                   IF        VTFUD <> §ISVVERS
009800040319     C                   MOVEL     '1'           ERRL00
009900040823     C                   EVAL      MESSAGGIO = MSG(24)
010000040319     C                   ENDIF
010100040823     C* Se occorso errore listo record corrente in stampa
010200040823     C                   IF        ERRL00 = '1'
010300040823     C                   EXCEPT    DET
010400040823     C                   MOVEL     '0'           ERRL00
010500040823     C                   ENDIF
010600001120     C*--------------------
010700011016     C* Programma traduttore
010800001120     C*--------------------
010900040823     C                   IF        VIRPGT <> *blanks
011000001120     C                   CALL(e)   'TIS721C'
011100040823     C                   PARM      VIRPGT        OBJECT           10
011200001120     C                   PARM      '*PGM   '     TYPE              7
011300001120     C                   PARM      '0'           ESITO             1
011400001120     C                   IF        ESITO = '1'
011500001120     C                   MOVEL     '1'           ERRL00
011600040823     C                   EVAL      MESSAGGIO = VIRPGT + ' ' + MSG(19)
011700001120     C                   ENDIF
011800040823     C* Se occorso errore listo record corrente in stampa
011900040823     C                   IF        ERRL00 = '1'
012000040823     C                   EXCEPT    DET
012100040823     C                   MOVEL     '0'           ERRL00
012200040823     C                   ENDIF
012300040823     C                   ENDIF
012400001120     C*--------------------
012500001120     C* Programma post-traduzione
012600001120     C*--------------------
012700040823     C                   IF        VIRPGI <> *blanks
012800001120     C                   CALL(e)   'TIS721C'
012900040823     C                   PARM      VIRPGI        OBJECT           10
013000001120     C                   PARM      '*PGM   '     TYPE              7
013100001120     C                   PARM      '0'           ESITO             1
013200001120     C                   IF        ESITO = '1'
013300001120     C                   MOVEL     '1'           ERRL00
013400040823     C                   EVAL      MESSAGGIO = VIRPGI + ' ' + MSG(19)
013500001120     C                   ENDIF
013600040823     C* Se occorso errore listo record corrente in stampa
013700040823     C                   IF        ERRL00 = '1'
013800040823     C                   EXCEPT    DET
013900040823     C                   MOVEL     '0'           ERRL00
014000040823     C                   ENDIF
014100040823     C                   ENDIF
014200001120     C*--------------------
014300001120     C* File da tradurre
014400001120     C*--------------------
014500040823     C                   IF        VIRFID <> *blanks
014600001120     C                   CALL(e)   'TIS721C'
014700040823     C                   PARM      VIRFID        OBJECT           10
014800001120     C                   PARM      '*FILE  '     TYPE              7
014900001120     C                   PARM      '0'           ESITO             1
015000001120     C                   IF        ESITO = '1'
015100001120     C                   MOVEL     '1'           ERRL00
015200040823     C                   EVAL      MESSAGGIO = VIRFID + ' ' + MSG(20)
015300001120     C                   ENDIF
015400040823     C* Se occorso errore listo record corrente in stampa
015500040823     C                   IF        ERRL00 = '1'
015600040823     C                   EXCEPT    DET
015700040823     C                   MOVEL     '0'           ERRL00
015800040823     C                   ENDIF
015900040823     C                   ENDIF
016000001120     C*--------------------
016100001120     C* File storico
016200001120     C*--------------------
016300040823     C                   IF        VIRFIS <> *blanks
016400001120     C                   CALL(e)   'TIS721C'
016500040823     C                   PARM      VIRFIS        OBJECT           10
016600001120     C                   PARM      '*FILE  '     TYPE              7
016700001120     C                   PARM      '0'           ESITO             1
016800001120     C                   IF        ESITO = '1'
016900001120     C                   MOVEL     '1'           ERRL00
017000040823     C                   EVAL      MESSAGGIO = VIRFIS + ' ' + MSG(20)
017100001120     C                   ENDIF
017200040823     C* Se occorso errore listo record corrente in stampa
017300040823     C                   IF        ERRL00 = '1'
017400040823     C                   EXCEPT    DET
017500040823     C                   MOVEL     '0'           ERRL00
017600040823     C                   ENDIF
017700040823     C                   ENDIF
017800001120     C*--------------------
017900001120     C* File tradotto
018000001120     C*--------------------
018100040823     C                   IF        VIRFIT <> *blanks
018200001120     C                   CALL(e)   'TIS721C'
018300040823     C                   PARM      VIRFIT        OBJECT           10
018400001120     C                   PARM      '*FILE  '     TYPE              7
018500001120     C                   PARM      '0'           ESITO             1
018600001120     C                   IF        ESITO = '1'
018700001120     C                   MOVEL     '1'           ERRL00
018800040823     C                   EVAL      MESSAGGIO = VIRFIT + ' ' + MSG(20)
018900001120     C                   ENDIF
019000040823     C* Se occorso errore listo record corrente in stampa
019100040823     C                   IF        ERRL00 = '1'
019200040823     C                   EXCEPT    DET
019300040823     C                   MOVEL     '0'           ERRL00
019400040823     C                   ENDIF
019500040823     C                   ENDIF
019600001120     C*--------------------
019700001120     C* Libreria file tradotto
019800001120     C*--------------------
019900040823     C                   IF        VIRFI3 <> *blanks
020000001120     C                   CALL(e)   'TIS721C'
020100040823     C                   PARM      VIRFI3        OBJECT           10
020200001120     C                   PARM      '*LIB   '     TYPE              7
020300001120     C                   PARM      '0'           ESITO             1
020400001120     C                   IF        ESITO = '1'
020500001120     C                   MOVEL     '1'           ERRL00
020600040823     C                   EVAL      MESSAGGIO = VIRFI3 + ' ' + MSG(21)
020700001120     C                   ENDIF
020800040823     C* Se occorso errore listo record corrente in stampa
020900040823     C                   IF        ERRL00 = '1'
021000040823     C                   EXCEPT    DET
021100040823     C                   MOVEL     '0'           ERRL00
021200040823     C                   ENDIF
021300040823     C                   ENDIF
021400040816     C*--------------------
021500040816     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto
021600040816     C*--------------------
021700040823     C     VIRFIT        IFne      *blank
021800040823     C     VIRPGT        ORne      *blank
021900040816     C                   EVAL      TBEcod = 'IFT'
022000040816     C     KEYtbe1_P     SETLL     tntbe01l
022100040816     C                   IF        %equal(tntbe01l)
022200040816     C* Inizializzo indici e schiere
022300040816     C                   Z-ADD     *zeros        j
022400040816     C                   CLEAR                   skTipF
022500040816     C                   CLEAR                   skPgt
022600040816     C                   CLEAR                   skFileT
022700040816     C* Carico le schiere da tabella 'IFT'
022800040816     C     KEYtbe1_P     READE     tntbe01l
022900040816     C                   DOW       not %eof(tntbe01l)
023000040816     C                   IF        TBEatb = *blanks
023100040816     C                   EVAL      DIFT = TBEuni
023200040816     C                   ADD       1             j
023300040816     C                   EVAL      skTipF(j)  = TBEke1
023400040816     C                   EVAL      skPgt(j)   = §IFTPGT
023500040816     C                   EVAL      skFileT(j) = §IFTFILE
023600040816     C                   ENDIF
023700040816     C     KEYtbe1_P     READE     tntbe01l
023800040816     C                   ENDDO
023900040816     C* Imposto cosa testare
024000040823     C                   IF        VIRPGT <> *blanks
024100040823     C                   EVAL      wVIRPGT = VIRPGT
024200040816     C                   ELSE
024300040823     C                   EVAL      wVIRPGT = VTFPGT
024400040816     C                   ENDIF
024500040823     C                   IF        VIRFIT <> *blanks
024600040823     C                   EVAL      wVIRFIT = VIRFIT
024700040816     C                   ELSE
024800040823     C                   EVAL      wVIRFIT = VTFFIT
024900040816     C                   ENDIF
025000040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
025100040823     C                   MOVEL     '0'           wFlgIFTok         1
025200040823     C     wVIRPGT       LOOKUP    skPgt                                  15
025300040816     C                   IF        %found
025400040816     C* Testo se vi è corrispondenza
025500040816     C                   EVAL      j = 1
025600040816     C                   DOW       j <= %elem(skTipF)
025700040823     C                   IF        skTipF(j)  = VIRTIP  AND
025800040823     C                             skPgt(j)   = wVIRPGT AND
025900040823     C                             skFileT(j) = wVIRFIT
026000040816     C                   MOVEL     '1'           wFlgIFTok
026100040816     C                   LEAVE
026200040816     C                   ENDIF
026300040816     C                   ADD       1             j
026400040816     C                   ENDDO
026500040816     C* Se nn trovata esatta corrispondenza => errore
026600040816     C                   IF        wFlgIFTok = '0'
026700040816     C                   MOVEL     '1'           ERRL00
026800040823     C                   EVAL      MESSAGGIO = MSG(25)
026900040816     C                   ENDIF
027000040816     C                   ENDIF
027100040823     C* Se occorso errore listo record corrente in stampa
027200040823     C                   IF        ERRL00 = '1'
027300040823     C                   EXCEPT    DET
027400040823     C                   MOVEL     '0'           ERRL00
027500040823     C                   ENDIF
027600040823     C                   ENDIF
027700040823     C                   ENDIF
027800001120     C*
027900001120     C                   ENDSR
028000000626     C*------------------------------------------------------------------------*
028100000626     C* Cntisv - CONTROLLA codice tipo legame
028200000626     C*------------------------------------------------------------------------*
028300001120     C     cntisv        begsr
028400040823     C*
028500001120     C                   clear                   TIBS02DS
028600040823     C*
028700001120     C                   eval      t02tla = 'L'
028800001120     C                   movel     'C'           t02mod
028900040823     C                   movel(p)  virfi1        t02ke1
029000000626     C                   movel     'ISV'         t02cod
029100001120     C                   movel     knsif         t02sif
029200040823     C                   call      'TIBS02R'
029300001120     C                   parm                    KPJBA
029400001120     C                   parm                    TIBS02DS
029500001120     C
029600001120     C*
029700040823     C                   if        T02err = *blanks
029800030603     C                   movel(P)  T02uni        DISV
029900001120     C                   else
030000001120     C                   movel     '1'           errl00
030100040823     C                   eval      messaggio = MSG(18)
030200040823     C                   endif
030300001120     C
030400001120     C                   endsr
030500991027     C*------------------------------------------------------------------------*
030600991027     C* cnttip - Controlla esistenza tipo file su tivt00f
030700991027     C*------------------------------------------------------------------------*
030800001120     C     cnttip        begsr
030900040823     C*
031000001120     C                   clear                   TIBS02DS
031100040823     C*
031200001120     C                   eval      t02tla = 'L'
031300040823     C                   movel     'C'           t02mod
031400040823     C                   movel(p)  virtip        t02ke1
031500991108     C                   movel     'ITF'         t02cod
031600001120     C                   movel     knsif         t02sif
031700040823     C                   call      'TIBS02R'
031800001120     C                   parm                    KPJBA
031900001120     C                   parm                    TIBS02DS
032000001120     C
032100001120     C*
032200040823     C                   if        T02err = *blanks
032300001120     C                   else
032400001120     C                   movel     '1'           errl00
032500040823     C                   eval      messaggio = msg(11)
032600001120     C                   endif
032700001120     C*
032800001120     C                   endsr
032900970526     C*------------------------------------------------------------------------*
033000040823     C* *INZSR - OPERAZIONI INIZIALI
033100970526     C*------------------------------------------------------------------------*
033200970526     C     *INZSR        BEGSR
033300970526     C*---
033400970526     C* RICEVIMENTO PARAMETRI
033500970526     C*---
033600970526     C     *ENTRY        PLIST
033700970612     C                   PARM                    KPJBA
033800970526     C*---
033900970526     C* VARIABILI RIFERITE AL DATA BASE
034000970526     C*---
034100991026     C     *LIKE         DEFINE    virtip        KCEtip
034200991026     C     *LIKE         DEFINE    virksc        KCEksc
034300000420     C     *LIKE         DEFINE    virdti        KCEdti
034400970526     C*---
034500970526     C* CHIAVI DI LETTURA
034600970526     C*---
034700040816     C* LETTURA tntbe01L - PARZIALE
034800040816     C     KEYtbe1_P     KLIST
034900040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
035000970526     C*
035100970526     C                   ENDSR
035200040823     O*------------------------------------------------------------------------*
035300040823     O* STAMPA
035400040823     O*------------------------------------------------------------------------*
035500040823     OPRTF198   E            DET         1
035600040823     O                       VIRKSC              10
035700040823     O                       VIRTIP              20
035800040823     O                       MESSAGGIO          140
035900040823     O                       PAGE          Z    198
036000040823** MSG - MESSAGGI
036100040823TIS723R- Tasto funzionale non gestito                                          1
036200040823TIS723R- Inserimento effettuato                                                2
036300040823TIS723R- Modifica effettuata                                                   3
036400040823TIS723R- Annullamento effettuato                                               4
036500040823TIS723R- Ripristino effettuato                                                 5
036600040823TIS723R- Richiesta funzione per record inesistente                             6
036700040823TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
036800040823TIS723R- Richiesta funzione per record esistente                               8
036900040823TIS723R- Questo codice cliente è già presente con questo tipo file             9
037000040823TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
037100040823TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
037200040823TIS723R- Campo obbligatorio                                                    12
037300040823TIS723R- Codice filiale inesistente o di categoria V=varia                     13
037400040823TIS723R- Data errata                                                           14
037500040823TIS723R- La data fine deve essere maggiore della data inizio                   15
037600040823TIS723R- Il record non può scadere in un giorno già trascorso                  16
037700040823TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
037800040823TIS723R- Codice tipo servizio non presente in tabella                          18
037900040823TIS723R- Oggetto inesistente, verificare e correggere.                         19
038000040823TIS723R- File inesistente, verificare e correggere.                            20
038100040823TIS723R- Libreria inesistente, verificare e correggere.                        21
038200040823TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
038300040823TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
038400040823TIS723R- Tipo file e servizio incompatibili, correggere.                       24
038500040823TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
