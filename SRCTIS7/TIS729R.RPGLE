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
011100050112     C                   PARM      *blanks       LIBRERIA         10
011200040823     C                   PARM      VIRPGT        OBJECT           10
011300001120     C                   PARM      '*PGM   '     TYPE              7
011400001120     C                   PARM      '0'           ESITO             1
011500001120     C                   IF        ESITO = '1'
011600001120     C                   MOVEL     '1'           ERRL00
011700040823     C                   EVAL      MESSAGGIO = VIRPGT + ' ' + MSG(19)
011800001120     C                   ENDIF
011900040823     C* Se occorso errore listo record corrente in stampa
012000040823     C                   IF        ERRL00 = '1'
012100040823     C                   EXCEPT    DET
012200040823     C                   MOVEL     '0'           ERRL00
012300040823     C                   ENDIF
012400040823     C                   ENDIF
012500001120     C*--------------------
012600001120     C* Programma post-traduzione
012700001120     C*--------------------
012800040823     C                   IF        VIRPGI <> *blanks
012900001120     C                   CALL(e)   'TIS721C'
013000050112     C                   PARM      *blanks       LIBRERIA         10
013100040823     C                   PARM      VIRPGI        OBJECT           10
013200001120     C                   PARM      '*PGM   '     TYPE              7
013300001120     C                   PARM      '0'           ESITO             1
013400001120     C                   IF        ESITO = '1'
013500001120     C                   MOVEL     '1'           ERRL00
013600040823     C                   EVAL      MESSAGGIO = VIRPGI + ' ' + MSG(19)
013700001120     C                   ENDIF
013800040823     C* Se occorso errore listo record corrente in stampa
013900040823     C                   IF        ERRL00 = '1'
014000040823     C                   EXCEPT    DET
014100040823     C                   MOVEL     '0'           ERRL00
014200040823     C                   ENDIF
014300040823     C                   ENDIF
014400001120     C*--------------------
014500001120     C* File da tradurre
014600001120     C*--------------------
014700040823     C                   IF        VIRFID <> *blanks
014800001120     C                   CALL(e)   'TIS721C'
014900050112     C                   PARM      *blanks       LIBRERIA         10
015000040823     C                   PARM      VIRFID        OBJECT           10
015100001120     C                   PARM      '*FILE  '     TYPE              7
015200001120     C                   PARM      '0'           ESITO             1
015300001120     C                   IF        ESITO = '1'
015400001120     C                   MOVEL     '1'           ERRL00
015500040823     C                   EVAL      MESSAGGIO = VIRFID + ' ' + MSG(20)
015600001120     C                   ENDIF
015700040823     C* Se occorso errore listo record corrente in stampa
015800040823     C                   IF        ERRL00 = '1'
015900040823     C                   EXCEPT    DET
016000040823     C                   MOVEL     '0'           ERRL00
016100040823     C                   ENDIF
016200040823     C                   ENDIF
016300001120     C*--------------------
016400001120     C* File storico
016500001120     C*--------------------
016600040823     C                   IF        VIRFIS <> *blanks
016700001120     C                   CALL(e)   'TIS721C'
016800050112     C                   PARM      *blanks       LIBRERIA         10
016900040823     C                   PARM      VIRFIS        OBJECT           10
017000001120     C                   PARM      '*FILE  '     TYPE              7
017100001120     C                   PARM      '0'           ESITO             1
017200001120     C                   IF        ESITO = '1'
017300001120     C                   MOVEL     '1'           ERRL00
017400040823     C                   EVAL      MESSAGGIO = VIRFIS + ' ' + MSG(20)
017500001120     C                   ENDIF
017600040823     C* Se occorso errore listo record corrente in stampa
017700040823     C                   IF        ERRL00 = '1'
017800040823     C                   EXCEPT    DET
017900040823     C                   MOVEL     '0'           ERRL00
018000040823     C                   ENDIF
018100040823     C                   ENDIF
018200001120     C*--------------------
018300001120     C* File tradotto
018400001120     C*--------------------
018500040823     C                   IF        VIRFIT <> *blanks
018600001120     C                   CALL(e)   'TIS721C'
018700050112     C                   PARM      VIRFI3        LIBRERIA         10
018800040823     C                   PARM      VIRFIT        OBJECT           10
018900001120     C                   PARM      '*FILE  '     TYPE              7
019000001120     C                   PARM      '0'           ESITO             1
019100001120     C                   IF        ESITO = '1'
019200001120     C                   MOVEL     '1'           ERRL00
019300040823     C                   EVAL      MESSAGGIO = VIRFIT + ' ' + MSG(20)
019400001120     C                   ENDIF
019500040823     C* Se occorso errore listo record corrente in stampa
019600040823     C                   IF        ERRL00 = '1'
019700040823     C                   EXCEPT    DET
019800040823     C                   MOVEL     '0'           ERRL00
019900040823     C                   ENDIF
020000040823     C                   ENDIF
020100001120     C*--------------------
020200001120     C* Libreria file tradotto
020300001120     C*--------------------
020400040823     C                   IF        VIRFI3 <> *blanks
020500001120     C                   CALL(e)   'TIS721C'
020600050112     C                   PARM      *blanks       LIBRERIA         10
020700040823     C                   PARM      VIRFI3        OBJECT           10
020800001120     C                   PARM      '*LIB   '     TYPE              7
020900001120     C                   PARM      '0'           ESITO             1
021000001120     C                   IF        ESITO = '1'
021100001120     C                   MOVEL     '1'           ERRL00
021200040823     C                   EVAL      MESSAGGIO = VIRFI3 + ' ' + MSG(21)
021300001120     C                   ENDIF
021400040823     C* Se occorso errore listo record corrente in stampa
021500040823     C                   IF        ERRL00 = '1'
021600040823     C                   EXCEPT    DET
021700040823     C                   MOVEL     '0'           ERRL00
021800040823     C                   ENDIF
021900040823     C                   ENDIF
022000040816     C*--------------------
022100040816     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto
022200040816     C*--------------------
022300040823     C     VIRFIT        IFne      *blank
022400040823     C     VIRPGT        ORne      *blank
022500040816     C                   EVAL      TBEcod = 'IFT'
022600040816     C     KEYtbe1_P     SETLL     tntbe01l
022700040816     C                   IF        %equal(tntbe01l)
022800040816     C* Inizializzo indici e schiere
022900040816     C                   Z-ADD     *zeros        j
023000040816     C                   CLEAR                   skTipF
023100040816     C                   CLEAR                   skPgt
023200040816     C                   CLEAR                   skFileT
023300040816     C* Carico le schiere da tabella 'IFT'
023400040816     C     KEYtbe1_P     READE     tntbe01l
023500040816     C                   DOW       not %eof(tntbe01l)
023600040816     C                   IF        TBEatb = *blanks
023700040816     C                   EVAL      DIFT = TBEuni
023800040816     C                   ADD       1             j
023900040816     C                   EVAL      skTipF(j)  = TBEke1
024000040816     C                   EVAL      skPgt(j)   = §IFTPGT
024100040816     C                   EVAL      skFileT(j) = §IFTFILE
024200040816     C                   ENDIF
024300040816     C     KEYtbe1_P     READE     tntbe01l
024400040816     C                   ENDDO
024500040816     C* Imposto cosa testare
024600040823     C                   IF        VIRPGT <> *blanks
024700040823     C                   EVAL      wVIRPGT = VIRPGT
024800040816     C                   ELSE
024900040823     C                   EVAL      wVIRPGT = VTFPGT
025000040816     C                   ENDIF
025100040823     C                   IF        VIRFIT <> *blanks
025200040823     C                   EVAL      wVIRFIT = VIRFIT
025300040816     C                   ELSE
025400040823     C                   EVAL      wVIRFIT = VTFFIT
025500040816     C                   ENDIF
025600040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
025700040823     C                   MOVEL     '0'           wFlgIFTok         1
025800040823     C     wVIRPGT       LOOKUP    skPgt                                  15
025900040816     C                   IF        %found
026000040816     C* Testo se vi è corrispondenza
026100040816     C                   EVAL      j = 1
026200040816     C                   DOW       j <= %elem(skTipF)
026300040823     C                   IF        skTipF(j)  = VIRTIP  AND
026400040823     C                             skPgt(j)   = wVIRPGT AND
026500040823     C                             skFileT(j) = wVIRFIT
026600040816     C                   MOVEL     '1'           wFlgIFTok
026700040816     C                   LEAVE
026800040816     C                   ENDIF
026900040816     C                   ADD       1             j
027000040816     C                   ENDDO
027100040816     C* Se nn trovata esatta corrispondenza => errore
027200040816     C                   IF        wFlgIFTok = '0'
027300040816     C                   MOVEL     '1'           ERRL00
027400040823     C                   EVAL      MESSAGGIO = MSG(25)
027500040816     C                   ENDIF
027600040816     C                   ENDIF
027700040823     C* Se occorso errore listo record corrente in stampa
027800040823     C                   IF        ERRL00 = '1'
027900040823     C                   EXCEPT    DET
028000040823     C                   MOVEL     '0'           ERRL00
028100040823     C                   ENDIF
028200040823     C                   ENDIF
028300040823     C                   ENDIF
028400001120     C*
028500001120     C                   ENDSR
028600000626     C*------------------------------------------------------------------------*
028700000626     C* Cntisv - CONTROLLA codice tipo legame
028800000626     C*------------------------------------------------------------------------*
028900001120     C     cntisv        begsr
029000040823     C*
029100001120     C                   clear                   TIBS02DS
029200040823     C*
029300001120     C                   eval      t02tla = 'L'
029400001120     C                   movel     'C'           t02mod
029500040823     C                   movel(p)  virfi1        t02ke1
029600000626     C                   movel     'ISV'         t02cod
029700001120     C                   movel     knsif         t02sif
029800040823     C                   call      'TIBS02R'
029900001120     C                   parm                    KPJBA
030000001120     C                   parm                    TIBS02DS
030100001120     C
030200001120     C*
030300040823     C                   if        T02err = *blanks
030400030603     C                   movel(P)  T02uni        DISV
030500001120     C                   else
030600001120     C                   movel     '1'           errl00
030700040823     C                   eval      messaggio = MSG(18)
030800040823     C                   endif
030900001120     C
031000001120     C                   endsr
031100991027     C*------------------------------------------------------------------------*
031200991027     C* cnttip - Controlla esistenza tipo file su tivt00f
031300991027     C*------------------------------------------------------------------------*
031400001120     C     cnttip        begsr
031500040823     C*
031600001120     C                   clear                   TIBS02DS
031700040823     C*
031800001120     C                   eval      t02tla = 'L'
031900040823     C                   movel     'C'           t02mod
032000040823     C                   movel(p)  virtip        t02ke1
032100991108     C                   movel     'ITF'         t02cod
032200001120     C                   movel     knsif         t02sif
032300040823     C                   call      'TIBS02R'
032400001120     C                   parm                    KPJBA
032500001120     C                   parm                    TIBS02DS
032600001120     C
032700001120     C*
032800040823     C                   if        T02err = *blanks
032900001120     C                   else
033000001120     C                   movel     '1'           errl00
033100040823     C                   eval      messaggio = msg(11)
033200001120     C                   endif
033300001120     C*
033400001120     C                   endsr
033500970526     C*------------------------------------------------------------------------*
033600040823     C* *INZSR - OPERAZIONI INIZIALI
033700970526     C*------------------------------------------------------------------------*
033800970526     C     *INZSR        BEGSR
033900970526     C*---
034000970526     C* RICEVIMENTO PARAMETRI
034100970526     C*---
034200970526     C     *ENTRY        PLIST
034300970612     C                   PARM                    KPJBA
034400970526     C*---
034500970526     C* VARIABILI RIFERITE AL DATA BASE
034600970526     C*---
034700991026     C     *LIKE         DEFINE    virtip        KCEtip
034800991026     C     *LIKE         DEFINE    virksc        KCEksc
034900000420     C     *LIKE         DEFINE    virdti        KCEdti
035000970526     C*---
035100970526     C* CHIAVI DI LETTURA
035200970526     C*---
035300040816     C* LETTURA tntbe01L - PARZIALE
035400040816     C     KEYtbe1_P     KLIST
035500040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
035600970526     C*
035700970526     C                   ENDSR
035800040823     O*------------------------------------------------------------------------*
035900040823     O* STAMPA
036000040823     O*------------------------------------------------------------------------*
036100040823     OPRTF198   E            DET         1
036200040823     O                       VIRKSC              10
036300040823     O                       VIRTIP              20
036400040823     O                       MESSAGGIO          140
036500040823     O                       PAGE          Z    198
036600040823** MSG - MESSAGGI
036700040823TIS723R- Tasto funzionale non gestito                                          1
036800040823TIS723R- Inserimento effettuato                                                2
036900040823TIS723R- Modifica effettuata                                                   3
037000040823TIS723R- Annullamento effettuato                                               4
037100040823TIS723R- Ripristino effettuato                                                 5
037200040823TIS723R- Richiesta funzione per record inesistente                             6
037300040823TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
037400040823TIS723R- Richiesta funzione per record esistente                               8
037500040823TIS723R- Questo codice cliente è già presente con questo tipo file             9
037600040823TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
037700040823TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
037800040823TIS723R- Campo obbligatorio                                                    12
037900040823TIS723R- Codice filiale inesistente o di categoria V=varia                     13
038000040823TIS723R- Data errata                                                           14
038100040823TIS723R- La data fine deve essere maggiore della data inizio                   15
038200040823TIS723R- Il record non può scadere in un giorno già trascorso                  16
038300040823TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
038400040823TIS723R- Codice tipo servizio non presente in tabella                          18
038500040823TIS723R- Oggetto inesistente, verificare e correggere.                         19
038600040823TIS723R- File inesistente, verificare e correggere.                            20
038700040823TIS723R- Libreria inesistente, verificare e correggere.                        21
038800040823TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
038900040823TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
039000040823TIS723R- Tipo file e servizio incompatibili, correggere.                       24
039100040823TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
