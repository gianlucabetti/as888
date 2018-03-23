000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------*
000300020613     H* STATISTICA EVENTI CLIENTE WIND
000400010509     H*-------------------------------------------------------------------------*
000500020614     FTITAS31C  IF   E           K DISK
000600010131     F                                     rename(titas000:titasx00)
000700010131     F                                     rename(titas010:titasx10)
000800010131     F                                     rename(titasP00:titasxP0)
000900030603     FFNLBL01L  IF   E           K DISK
001000030414     FFNEVB05L  IF   E           K DISK
001100030414     FTABEL00F  IF   E           K DISK
001200030414     FTIST57P   O    E             PRINTER OFLIND(*IN01)
001300001218     D*--------------------
001400001218     D* DS ESTERNE
001500001218     D*--------------------
001600900517     D KPJBA         E DS
001700011112     D*--------------------
001800011112     D* DS DI RIDEFINIZIOINE CAMPO DATA SPEDIZIONE TAS
001900011112     D*--------------------
002000011112     D                 DS
002100011112     D  TASAAS                        4  0
002200011112     D  TASMGS                        4  0
002300011112     D  TASDSP                 1      8  0
002400020613     D*------------------
002500020613     D* DS X PASSAGGIO PARAMETRI
002600020613     D*------------------
002700020613     D PARAM           DS                  INZ
002800020613     D  PARKSC                 1      7  0
002900020613     D  PARDAI                 8     15  0
003000020613     D  PARDAF                16     23  0
003100030414     D  PARDAI2               24     31  0
003200030414     D  PARDAF2               32     39  0
003300030414     D  PAROTQ                40     49
003400030414     D  PARRAG                50     89
003500030414     D*--------------------
003600030414     D* VARIABILI DI WRK
003700030414     D*--------------------
003800030414     D depCEV          s                   like(evbCEV)
003900030414     D wPriorita       s              1  0 inz(*zeros)
004000030414     D wDepPriorita    s              1  0 inz(*zeros)
004100030414     D ds2a          e ds
004200030414     D sk2Ainderr      s              3    dim(5) ctdata perrcd(1)
004300030414     D sk2Arif         s              3    dim(20) ctdata perrcd(1)
004400030414     D sk2Agiac        s              3    dim(100)
004500030414     D*---
004600030414     D* DS REPERIMENTO DATI UTENTE
004700030414     D*---
004800030414     D TIBS34DS      E DS                                                       *Profili utente
004900030414     D DDATIUTE      E DS                                                       *Dati utente
005000030414     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005100001218
005200001218
005300001218
005400920812     C*---------------------------------------------------------------*
005500001218     C* MAIN
005600001218     C*---------------------------------------------------------------*
005700030414     C                   exsr      cartab
005800001218     C                   exsr      procedi
005900030414     C                   exsr      stampa
006000001218     C*
006100001218     C                   seton                                        LR
006200010509     C*---------------------------------------------------------------*
006300001218
006400001218
006500001218
006600001218     C*------------------------------------------------------------------------*
006700001218     C* PROCEDI - Routine principale
006800001218     C*------------------------------------------------------------------------*
006900020613     C     PROCEDI       BEGSR
007000001218     C*
007100020614     C* Mi posiziono sul TITAS31C per cliente fatturazione
007200020613     C                   Z-ADD     PARKSC        tasKSC
007300030414     C     KEYtas31cP    SETLL     TITAS31C
007400020613     C*
007500020613     C* Se posizionamento riuscito => ciclo ed elaboro
007600020614     C                   IF        %found(TITAS31C)
007700020614     C                   MOVEL     '0'           flgFINE           1
007800020613     C*
007900020613     C* Prima lettura fuori ciclo
008000020613     C                   EXSR      LEGGITAS
008100020614     C                   DOW       not %eof(TITAS31C) AND
008200020614     C                             flgFINE = '0'
008300020613     C*
008400020613     C* Verifico se la bolla è da considerare
008500020613     C                   EXSR      CHKRECTAS
008600020613     C                   IF        flgRECOK = 'S'
008700020613     C*
008800030414     C* Se la bolla è da considerare => totalizzo spedizione
008900030414     C                   EXSR      MEMSPE
009000020613     C                   ENDIF
009100020613     C*
009200020613     C* Proseguo le letture dentro ciclo
009300020613     C                   EXSR      LEGGITAS
009400020613     C                   ENDDO
009500001218     C*
009600020613     C                   ENDIF
009700020613     C*
009800001218     C                   ENDSR
009900001218     C*------------------------------------------------------------------------*
010000020613
010100020613
010200020613
010300020613     C*------------------------------------------------------------------------*
010400030414     C* LEGGITAS - Routine di lettura record TITAS31C
010500020613     C*------------------------------------------------------------------------*
010600020613     C     LEGGITAS      BEGSR
010700020613     C*
010800020614     C                   READ      TITAS31C
010900020613     C*
011000020613     C                   ENDSR
011100020613     C*------------------------------------------------------------------------*
011200010627
011300010627
011400010627
011500010627     C*------------------------------------------------------------------------*
011600020613     C* CHKRECTAS - Routine di verifica bolla
011700010627     C*------------------------------------------------------------------------*
011800020613     C     CHKRECTAS     BEGSR
011900010627     C*
012000020613     C                   MOVEL     'S'           flgRECOK          1
012100020613     C*
012200020613     C* Testo se la bolla è del cliente richiesto
012300030603     C                   IF        flgRECOK = 'S'
012400020613     C                   IF        tasKSC = PARKSC
012500020613     C                   ELSE
012600020613     C                   MOVEL     'N'           flgRECOK
012700020614     C* quando cambia il cliente esco dal ciclo
012800020614     C                   MOVEL     '1'           flgFINE
012900020613     C                   ENDIF
013000030603     C                   ENDIF
013100020613     C*
013200020613     C* Testo se la bolla è del periodo richiesto
013300030603     C                   IF        flgRECOK = 'S'
013400020613     C                   IF        tasDSP >= PARDAI AND
013500020613     C                             tasDSP <= PARDAF
013600020613     C                   ELSE
013700020613     C                   MOVEL     'N'           flgRECOK
013800020613     C                   ENDIF
013900030603     C                   ENDIF
014000030603     C*
014100030603     C* Testo che la bolla nn sia 1 bolla "figlia"
014200030603     C                   IF        flgRECOK = 'S'
014300030603     C     KEYlbl01C     CHAIN     FNLBL01L
014400030603     C                   IF        %found(FNLBL01L)
014500030603     C                   MOVEL     'N'           flgRECOK
014600030603     C                   ENDIF
014700030603     C                   ENDIF
014800010627     C*
014900010627     C                   ENDSR
015000010627     C*------------------------------------------------------------------------*
015100010626
015200010626
015300010626
015400010626
015500010509     C*------------------------------------------------------------------------*
015600030414     C* MEMSPE - Routine di totalizzazione spedizioni
015700010509     C*------------------------------------------------------------------------*
015800030414     C     MEMSPE        BEGSR
015900010509     C*
016000030414     C*
016100030414     C* Innanzitutto elaboro solo le bolle "chiuse" con reso e aventi una apertura giacenza
016200030414 IF 1C                   IF        tasDCM > *zeros AND
016300030429     C                             tasCCA = '2'
016400030521     C*
016500030521     C* Elaboro solo le bolle di reso aventi numero distinta consegna <> 9999999
016600030711 IF 2C                   IF        tasNDC <> 9999999
016700030414     C*
016800030414     C* ...quindi verifico il tipo di evento da assegnare alla spedizione
016900030414     C* (tengo l'ultimo evento di tipo giacenza)
017000030414     C                   CLEAR                   depCEV
017100030414     C                   CLEAR                   wPriorita
017200030414     C                   CLEAR                   wDepPriorita
017300030414     C     KEYevb05P     SETLL     fnevb05l
017400030414     C     KEYevb05P     READE     fnevb05l
017500030414     C                   DOW       not %eof(fnevb05l)
017600030414     C     evbCEV        LOOKUP    sk2Agiac                               19
017700030414     C                   IF        *IN19 = *ON
017800030414     C                   IF        evbCEV = 'G'
017900030414     C                   EVAL      wPriorita = 0
018000030414     C                   ELSE
018100030414     C                   EVAL      wPriorita = 1
018200030414     C                   ENDIF
018300030414     C* Considero l'evento 'G  ' secondario rispetto a tutti gli altri
018400030414     C                   IF        wPriorita >= wDepPriorita
018500030414     C                   EVAL      wDepPriorita = wPriorita
018600030414     C                   EVAL      depCEV = evbCEV
018700030414     C                   ENDIF
018800030414     C                   ENDIF
018900030414     C     KEYevb05P     READE     fnevb05l
019000030414     C                   ENDDO
019100030414     C*
019200030414     C* ...distinguo quindi l'evento x identificare quale file scrivere
019300030711 IF 3C                   IF        depCEV <> *blanks
019400030414     C     depCEV        LOOKUP    sk2Ainderr                             20
019500030711 IF 4C                   IF        *IN20 = *ON
019600030414     C* => Indirizzi errati
019700030414     C                   EVAL      STPQNCIE = STPQNCIE + 1
019800030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
019900030711 X  4C                   ELSE
020000030414     C     depCEV        LOOKUP    sk2Arif                                21
020100030711 IF 5C                   IF        *IN21 = *ON
020200030414     C* => Rifiuti destinatario
020300030414     C                   EVAL      STPQNCR = STPQNCR + 1
020400030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
020500030711 X  5C                   ELSE
020600030414     C* => Altri motivi di reso
020700030414     C                   EVAL      STPQNCA = STPQNCA + 1
020800030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
020900030711 E  5C                   ENDIF
021000030711 E  4C                   ENDIF
021100030711 X  3C                   ELSE
021200030711     C* => Altri motivi di reso
021300030711     C                   EVAL      STPQNCA = STPQNCA + 1
021400030711     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
021500030711 E  3C                   ENDIF
021600030711     C                   ELSE
021700030711 X  2C* Se trattasi di resi "9999999" li conteggio come W.I.P. (Work In Progress)
021800030603     C                   EVAL      STPQIP = STPQIP + 1
021900030603     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
022000030711 E  2C                   ENDIF
022100030414     C*
022200030414 X  1C                   ELSE
022300030414     C*
022400030414     C* X le spedizioni OK...
022500030414 IF 2C                   IF        tasDCM > *zeros  AND
022600030414     C                             tasCCA = *blanks
022700030414     C                   EVAL      STPQCO = STPQCO + 1
022800030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
022900030414     C*
023000030414 END2C                   ENDIF
023100030414 END1C                   ENDIF
023200030414     C*
023300030414     C* Per le spedizioni nn ancora chiuse
023400030414 IF 3C                   IF        tasDCM = *zeros
023500030414     C                   EVAL      STPQIP = STPQIP + 1
023600030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
023700030414 END3C                   ENDIF
023800010509     C*
023900010509     C                   ENDSR
024000010509     C*---------------------------------------------------------------*
024100030414
024200030414
024300030414
024400030414
024500030414     C*------------------------------------------------------------------------*
024600030603     C* STAMPA - Routine di calcolo percentuali e stampa prospetto riepilogativo
024700030414     C*------------------------------------------------------------------------*
024800030414     C     STAMPA        BEGSR
024900030414     C*
025000030414     C* Solo se totalizzato qualcosa
025100030414     C                   IF        STPTOTSPE > *zeros
025200030414     C*
025300030414     C* % IN PROCESS
025400030414     C                   EVAL      STPPIP = STPQIP * 100 / STPTOTSPE
025500030414     C*
025600030414     C* % CONSEGNATE
025700030414     C                   EVAL      STPPCO = STPQCO * 100 / STPTOTSPE
025800030414     C*
025900030414     C* % NON CONSEGNATE - INDIRIZZI ERRATI
026000030414     C                   EVAL      STPPNCIE = STPQNCIE * 100 / STPTOTSPE
026100030414     C*                  - RIFIUTI
026200030414     C                   EVAL      STPPNCR = STPQNCR * 100 / STPTOTSPE
026300030414     C*                  - ALTRI MOTIVI
026400030414     C                   EVAL      STPPNCA = STPQNCA * 100 / STPTOTSPE
026500030414     C*
026600030414     C                   ENDIF
026700030414     C*
026800030414     C* IMPOSTO PARAMETRI DI LANCIO X TESTATA
026900030414     C                   EVAL      STPSYF = RSUT
027000030414     C                   EVAL      STPKSC = PARKSC
027100030414     C                   EVAL      STPRAG = PARRAG
027200030414     C                   EVAL      STPDAI = PARDAI2
027300030414     C                   EVAL      STPDAF = PARDAF2
027400030414     C*
027500030414     C* STAMPO TESTATA
027600030414     C                   WRITE     ST56PT
027700030414     C                   WRITE     ST56T1
027800030414     C                   WRITE     ST56PI
027900030414     C                   WRITE     ST56P2
028000030414     C                   WRITE     ST56RV
028100030414     C                   WRITE     ST56TT
028200030414     C                   WRITE     ST56RI
028300030414     C                   WRITE     ST56FI
028400030414     C*
028500030414     C                   ENDSR
028600030414     C*------------------------------------------------------------------------*
028700030414
028800030414
028900030414
029000030414     C*---------------------------------------------------------------*
029100030414     C* CARTAB - Caricamento tabelle di wrk
029200030414     C*---------------------------------------------------------------*
029300030414     C     CARTAB        BEGSR
029400030414     C*
029500030414     C                   clear                   sk2Agiac
029600030414     C                   z-add     *zeros        i                 3 0
029700030414     C                   eval      tblKUT = 1
029800030414     C                   eval      tblCOD = '2A'
029900030414     C     KEYtabP       setll     tabel00f
030000030414     C     KEYtabP       reade     tabel00f
030100030414     C                   dow       not %eof(tabel00f)
030200030414     C                   movel(p)  tblUNI        ds2a
030300030414     C                   if        §2AFTC = 'G'
030400030414     C                   add       1             i
030500030414     C                   eval      sk2Agiac(i) = tblKEY
030600030414     C                   endif
030700030414     C     KEYtabP       reade     tabel00f
030800030414     C                   enddo
030900030414     C*
031000030414     C                   ENDSR
031100030414     C*------------------------------------------------------------------------*
031200030414
031300030414
031400030414
031500030414
031600030414     C*------------------------------------------------------------------------*
031700030414     C* REPERISCE I DATI UTENTE
031800030414     C*------------------------------------------------------------------------*
031900030414     C     REPDATIUTE    BEGSR
032000030414     C*
032100030414     C* INIZIALIZZA VARIABILI DI WRK
032200030414     C                   CLEAR                   TIBS34DS
032300030414     C                   CLEAR                   AZUTEDS
032400030414     C                   CLEAR                   DDATIUTE
032500030414     C*
032600030414     C     *DTAARA       DEFINE    §azute        azuteds
032700030414     C     *DTAARA       DEFINE    §datiute      ddatiute
032800030414     C                   IN(E)     *DTAARA
032900030414if  1C                   IF        %Error
033000030414     c                   EVAL      I34Tla = 'L'
033100030414     C                   CALL      'TIBS34R'
033200030414     C                   PARM                    Tibs34Ds
033300030414     C                   IN        *DTAARA
033400030414e   1C                   ENDIF
033500030414     C*
033600030414     C                   ENDSR
033700030414     C*------------------------------------------------------------------------*
033800010509
033900010509
034000001218
034100001218     C*------------------------------------------------------------------------*
034200001218     C* *INZSR - ROUTINE INIZIALE
034300001218     C*------------------------------------------------------------------------*
034400001218     C     *INZSR        BEGSR
034500001218     C*
034600010726     C* Ricevo in input il parametro indicante l'ID LANCIO
034700010726     C     *ENTRY        PLIST
034800020613     C                   PARM                    KPJBA
034900020613     C*
035000020613     C                   MOVEL     KPJBU         PARAM
035100010726     C*
035200030414     C                   Z-ADD     1             tblKUT
035300030414     C****
035400030414     C* Definizione chiavi
035500030414     C****
035600030414     C* Chiave su TABEL00F - parziale
035700030414     C     KEYtabP       KLIST
035800030414     C                   KFLD                    tblKUT
035900030414     C                   KFLD                    tblCOD
036000030414     C* Chiave su TITAS31C - parziale
036100030414     C     KEYtas31cP    KLIST
036200030414     C                   KFLD                    tasKSC
036300030414     C* Chiave su FNEVB05L - parziale
036400030414     C     KEYevb05P     KLIST
036500030414     C                   KFLD                    tasAAS
036600030414     C                   KFLD                    tasLNP
036700030414     C                   KFLD                    tasNRS
036800030414     C                   KFLD                    tasNSP
036900030603     C* Chiave su FNLBL01L - completa
037000030603     C     KEYlbl01C     KLIST
037100030603     C                   KFLD                    tasAAS
037200030603     C                   KFLD                    tasLNP
037300030603     C                   KFLD                    tasNRS
037400030603     C                   KFLD                    tasNSP
037500030414     C*
037600030414     C* REPERIMEMTO DATI UTENTE
037700030414     C                   EXSR      REPDATIUTE
037800001218     C*
037900001218     C                   ENDSR
038000030414** sk2Ainderr - SCHIERA CODICI EVENTO X INDIRIZZO ERRATO
038100030414019
038200030414020
038300030414021
038400030414022
038500030414
038600030414** sk2Arif    - SCHIERA CODICI EVENTO X RIFIUTO DESTINATARIO
038700030414001
038800030414002
038900030414003
039000030414004
039100030414005
039200030414007
039300030414008
039400030414010
039500030414012
039600030414013
039700030414014
039800030414027
039900030414029
040000030414030
040100030414031
040200030414RFM
040300030414RFD
040400030414
040500030414
040600030414
